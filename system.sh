#!/bin/sh -e

[ "$(id -u)" = 0 ] || exec sudo $0

ROOT=$(cd "$(dirname "$0")"; pwd -P)

. $ROOT/lib.sh

##
## Base
##

if distro alpine; then
	pkg py3-jinja2 py3-yaml
elif distro arch; then
	pkg python-jinja python-yaml
fi

pkg sudo

if distro arch; then
	pkg pacman-contrib lostfiles openssh
fi

if distro alpine; then
	pkg openssh-client

	if role server; then
		pkg openssh-server
	fi
fi

if distro arch; then
	role vm || pkg fwupd
elif distro alpine; then
	role vm || pkg fwup
fi

if distro arch; then
	# Unneeded base packages:
	UNNEEDED_PKGS='
		dhcpcd
		haveged
		iotop
		jfsutils
		linux
		lsof
		lvm2
		mdadm
		mtr
		nano
		net-tools
		netctl
		psmisc
		reiserfsprogs
		s-nail
		sysstat
		systemd-sysvcompat
		whois
		xfsprogs
	'

	for p in $UNNEEDED_PKGS; do
		if pacman -Q $p >/dev/null 2>&1; then
			pacman -Rs $p
		fi
	done
	unset p
fi

if role desktop; then
	# Periodic TRIM:
	svc fstrim.timer
fi

if distro arch; then
	file /etc/pacman.conf
	file /etc/pacman.d/mirrorlist
	file /etc/pacman.d/hooks/needrestart.hook
fi

if distro alpine; then
	file /etc/apk/repositories
fi

if role desktop; then
	# Autologin to TTY 1:
	tmpl /etc/systemd/system/getty@tty1.service.d/override.conf
fi

if distro arch; then
	# Keep no pacman cache for uninstalled packages and 2 versions of
	# installed packages:
	file /etc/systemd/system/paccache.service.d/override.conf
	svc paccache.timer
fi

# Passwordless sudo for wheel:
echo '%wheel ALL = (ALL) NOPASSWD: ALL' > /etc/sudoers.d/wheel

##
## Net
##

if distro arch && role desktop; then
	file /etc/systemd/resolved.conf.d/static.conf
fi

##
## CLI
##

if role dev; then
	pkg '
		git
		vim
		bash-completion
		tmux
		the_silver_searcher
		fzy
		ncdu
	'

	if distro alpine; then
		pkg '
			less
			git-email
			git-perl
		'
	fi
fi

##
## Build
##

if distro arch; then
	pkg devtools
fi

if distro alpine && role build; then
	pkg alpine-sdk
fi

##
## Debug
##

if distro arch; then
	pkg ps_mem
fi

pkg htop

##
## Dev
##

if role work; then
	pkg '
		make
		go
	'
fi

##
## Sec
##

pkg nftables

tmpl /etc/nftables.conf
chmod 700 /etc/nftables.conf

if distro alpine; then
	file /etc/conf.d/nftables
fi

svc nftables

if distro arch; then
	pkg arch-audit
fi

chmod 700 /boot

file /etc/sysctl.d/50-dmesg-restrict.conf

##
## Desktop
##

if role desktop; then
	pkg '
		sway
		swaylock
		swayidle
		wl-clipboard
		mako
		libnotify
		light
		grim
		slurp
		xorg-server-xwayland
		i3status
		alacritty
		firefox
		imv
		noto-fonts
		ttf-ibm-plex
		unzip
	'

	file /etc/udev/rules.d/99-sway-monitor-hotplug.rules
	file /usr/local/bin/sway-monitor-hotplug.sh
	chmod +x /usr/local/bin/sway-monitor-hotplug.sh
	udevadm control --reload
fi

##
## Laptop
##

if role desktop; then
	file /etc/sysctl.d/disable_watchdog.conf
	file /etc/modprobe.d/audio_powersave.conf
fi

##
## Bluetooth
##

if role desktop; then
	pkg '
		bluez
		bluez-utils
	'

	file /etc/bluetooth/main.conf

	svc bluetooth
fi

##
## Media
##

if role desktop; then
	pkg '
		pulseaudio
		pulsemixer
		mpv
		libva-intel-driver
	'
fi

##
## Mail
##

if role mail; then
	pkg mutt
fi

##
## Bouncer
##

_w_run() {
	local f=/var/lib/weechat/weechat_fifo

	echo "$1" | sed 's/^/*/' > $f
}

_w_has() {
	local kv="$1"
	local cfg="$2"
	local r=/var/lib/weechat

	fgrep -q "$kv" $r/$cfg.conf
}

_w_set() {
	local set_key=$1
	local val="$2"
	local cfg=${set_key%%.*}
	local cfg_key=$(echo $set_key | cut -d. -f3-)

	if echo "$val" | egrep -vq '^[a-z0-9]{1,14}$'; then
		val="\"$val\""
	fi

	case  "$val" in
		$IRC_NICK|buflist)
			val="\"$val\""
			;;
	esac


	if ! _w_has "$cfg_key = $val" $cfg; then
		echo weechat $set_key $val
		_w_run "/set $set_key $val"
		_w_run '/save'
	fi
}

if role irc; then
	pkg weechat
	grep -q ^weechat: /etc/group ||
		addgroup -S weechat
	grep -q ^weechat: /etc/passwd ||
		adduser -S -D -H -h /var/lib/weechat -s /bin/sh \
			-G weechat -g weechat weechat

	mkdir -p /var/lib/weechat
	chown weechat: /var/lib/weechat
	chmod 750 /var/lib/weechat
	file /etc/init.d/weechat
	file /var/lib/weechat/.tmux.conf

	svc weechat

	_w_set weechat.plugin.autoload \
		'*,!script,!trigger,!xfer,!exec,!fset'

	_w_set weechat.color.chat_highlight white
	_w_set weechat.color.chat_highlight_bg red
	_w_set weechat.color.chat_nick_self 102

	_w_set weechat.look.buffer_time_format '${color:102}%H:%M'
	_w_set weechat.look.prefix_suffix ''
	_w_set weechat.look.prefix_align none
	_w_set weechat.look.align_end_of_lines prefix

	_w_set irc.look.highlight_channel '(?-i)$nick:,(?-i)$nick '
	_w_set irc.look.server_buffer independent

	_w_set weechat.bar.title.hidden on
	_w_set weechat.bar.status.hidden on
	_w_set weechat.bar.nicklist.hidden on
	_w_set weechat.bar.input.items \
		'>,[input_search],[input_paste],[scroll],input_text'

	_w_set weechat.bar.buflist.items buflist
	_w_set weechat.bar.buflist.position top
	_w_set weechat.bar.buflist.filling_top_bottom horizontal
	_w_set weechat.bar.buflist.separator off
	_w_set weechat.bar.buflist.color_bg 254
	_w_set buflist.format.buffer ' ${if:${current_buffer}?${color:255}:${color:default}}${format_number}${indent}${format_nick_prefix}${if:${current_buffer}?${color:255}:${color_hotlist}}${format_name} '
	_w_set buflist.format.buffer_current '${color:255,24}${format_buffer}'
	_w_set buflist.format.hotlist_low '${color:102}'
	_w_set buflist.format.indent ''
	_w_set buflist.format.number '${number}${if:${number_displayed}? :}'


	_w_set irc.server_default.msg_part ''
	_w_set irc.server_default.msg_quit ''
	_w_set irc.server_default.sasl_mechanism plain
	_w_set irc.server_default.sasl_username $IRC_NICK
	_w_set irc.server_default.nicks $IRC_NICK

	if ! _w_has 'freenode.autoconnect = on' irc; then
		_w_run '/server add freenode chat.freenode.net/6697 -ssl -autoconnect'
		_w_run '/save'
	fi
	_w_set irc.server.freenode.sasl_password $IRC_FREENODE_SASL
	_w_set irc.server.freenode.autojoin $IRC_FREENODE_JOIN

	_w_set irc.color.topic_new default
	_w_set irc.color.topic_old 102

	if ! _w_has 'irc_smart = on;*;irc_smart_filter;*' weechat; then
		_w_run '/filter add irc_smart * irc_smart_filter *'
		_w_run '/save'
	fi

	_w_set logger.look.backlog 0
	_w_set logger.file.mask '$plugin.$name.log'
	_w_set logger.mask.irc '$server-$channel-%Y-%m.log'
	_w_set logger.level.irc 1
fi

##
## Mailserver
##

if role mailsrv; then
	pkg '
		postfix
		dovecot
		dovecot-lmtpd
		dovecot-pigeonhole-plugin
	'

	tmpl /etc/postfix/main.cf
	tmpl /etc/postfix/aliases

	a=/etc/postfix/aliases
	if [ ! -e $a.db -o $a -nt $a.db ]; then
		echo Generating new $a.db
		newaliases
	fi
	unset a

	svc postfix

	file /etc/dovecot/conf.d/00-local.conf
	file /etc/dovecot/conf.d/99-local.conf

	svc dovecot
fi
