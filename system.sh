#!/bin/sh -e

[ "$(id -u)" = 0 ] || exec sudo $0 $(id -un )

UNPRIVUSR="$1"
shift

ROOT=$(cd "$(dirname "$0")"; pwd -P)

. $ROOT/lib.sh

##
## Base
##

pkg '
	python3-jinja2
	python3-yaml
	sudo
	curl
	ca-certificates
	unattended-upgrades
	python3-gi
	needrestart
'

# Passwordless sudo:
echo '%sudo ALL = (ALL) NOPASSWD: ALL' > /etc/sudoers.d/sudo-nopasswd

tmpl /etc/apt/sources.list

for f in norecommends autoremove periodicclean showversions; do
	file /etc/apt/apt.conf.d/$f
done
unset f

file /usr/local/bin/apt-backports
chmod +x /usr/local/bin/apt-backports

# Persistend systemd yournal:
mkdir -p /var/log/journal

pkg openssh-client
if role server; then
	pkg ssh
fi

if role vm; then
	pkg linux-image-cloud-amd64
else
	pkg fwupd intel-microcode
fi

svc systemd-timesyncd

##
## Net
##

if role desktop; then
	file /etc/systemd/resolved.conf.d/static.conf

	_wlif=$(ip a | awk '/^[0-9]: wl/ { print $2 }' | tr -d :)
	envfile /etc/wpa_supplicant/wpa_supplicant-${_wlif}.conf
fi

if role server; then
	tmpl /etc/systemd/network/wired.network
	svc systemd-networkd
fi

svc systemd-resolved
ln -sf /run/systemd/resolve/resolv.conf /etc/resolv.conf

##
## Sec
##

# TODO: switch to nftables when docker supports iptables-nft:
#       https://github.com/moby/moby/issues/38099
if ! role work; then
	pkg nftables

	tmpl /etc/nftables.conf
	chmod 700 /etc/nftables.conf

	svc nftables
fi

chmod 700 /boot

file /etc/sysctl.d/50-dmesg-restrict.conf

##
## Debug
##

(
	v=3.12
	u=https://raw.githubusercontent.com/pixelb/ps_mem/v$v/ps_mem.py
	b=/usr/local/bin/ps_mem
	if ! grep -q "^# V$v" $b 2>/dev/null; then
		curl -L $u > $b
			sed -i 's/env python/env python3/' $b
			chmod +x $b
	fi
	unset psv
)

pkg htop

##
## Dev
##

if role dev; then
	pkg '
		man-db
		git
		bash-completion
		tmux
		vim-nox
                ncurses-term
		fzy
		ncdu
		silversearcher-ag
		jq
	'
fi

if role work; then
	(
		v=1.12.5
		u=https://dl.google.com/go/go$v.linux-amd64.tar.gz

		if ! /opt/go/bin/go version | grep -q go$v 2>/dev/null; then
			curl -L $u | tar -C /opt -xz
		fi
	)
	pkg make go-bindata gcc libc6-dev libxml2-utils

	if ! apt-key list 2>/dev/null | grep -q docker@docker.com; then
		curl -fsSL https://download.docker.com/linux/debian/gpg | apt-key add -
	fi

	file /etc/apt/sources.list.d/docker.list
	pkg docker-ce

	if [ "$(readlink /etc/alternatives/iptables)" != /usr/sbin/iptables-legacy ]; then
		update-alternatives --set iptables /usr/sbin/iptables-legacy
	fi

	pkg virtualenv python3-dev libssl-dev libffi-dev

	if ! [ -e /opt/az/bin/python3 ]; then
		virtualenv -p python3 /opt/az
	fi

	pip /opt/az 'azure-cli==2.0.70'
	file /usr/local/bin/az
	chmod +x /usr/local/bin/az
	file /usr/share/bash-completion/completions/az

	pkg dnsutils
fi

##
## Desktop
##

if role desktop; then
	pkg '
		xserver-xorg-core
		xserver-xorg-input-libinput
		x11-xserver-utils
		xinit
		xdg-utils
		i3-wm
		i3lock
		py3status
		brightnessctl
		brightness-udev
		xterm
		maim
		sxiv
		mupdf
		redshift
		geoclue-2.0
		fonts-noto-core
		fonts-noto-mono
		fonts-noto-color-emoji
		fonts-ibm-plex
		bzip2
		libgtk-3-0
		libdbus-glib-1-2
	'

	# Periodic TRIM:
	svc fstrim.timer

	# Autologin to TTY 1:
	tmpl /etc/systemd/system/getty@tty1.service.d/override.conf

	file /etc/sysctl.d/disable_watchdog.conf
	file /etc/modprobe.d/audio_powersave.conf

	file /etc/X11/xorg.conf.d/00-keyboard.conf
	file /etc/X11/xorg.conf.d/00-touchpad.conf

	file /usr/local/bin/x-monitor-hotplug
	chmod +x /usr/local/bin/x-monitor-hotplug
	tmpl /etc/udev/rules.d/99-x-monitor-hotplug.rules
	udevadm control --reload

	tmpl /etc/systemd/system/i3lock.service
	systemctl enable i3lock
fi

if role dekstop || role media; then
	pkg snapd
fi

##
## Bluetooth
##

if role desktop; then
	pkg bluez

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
		i965-va-driver
		youtube-dl
	'

	# PMP:
	pkg '
		fuse
		libnss3
	'

	_pmpu=https://knapsu.eu/data/plex/latest
	_pmpf=$(curl -sI $_pmpu | awk '/^location: / { print $2 }' | tr -cd '[:alnum:]._-')
	if ! [ -e /opt/$_pmpf ]; then
		curl -L $_pmpu > /opt/$_pmpf
		chmod +x /opt/$_pmpf
		ln -sf /opt/$_pmpf /usr/local/bin/plex-media-player
	fi
fi

if role desktop || role media; then
	pkg lftp
fi

if role media; then
	pkg mediainfo
fi

##
## Mail
##

if role server; then
	pkg nullmailer bsd-mailx

	tmpl /etc/nullmailer/remotes
	tmpl /etc/nullmailer/adminaddr

	svc nullmailer
fi

##
## TLS
##

if role tls; then
	pkg dehydrated jq moreutils

	tmpl /etc/dehydrated/config
	tmpl /etc/dehydrated/domains.txt
	tmpl /etc/dehydrated/hooks/cf.sh
	chmod 700 /etc/dehydrated/hooks/cf.sh
	file /etc/cron.daily/dehydrated
	chmod 750 /etc/cron.daily/dehydrated
fi

##
## WWW
##

if role www; then
	pkg '
		nginx-light
		libnginx-mod-http-fancyindex
	'

	file /etc/nginx/ffdhe4096.pem
	file /etc/nginx/conf.d/default.conf
	file /etc/nginx/conf.d/ssl.part
	file /etc/logrotate.d/nginx
	rm -f  /etc/nginx/sites-enabled/default

	tmplexec <<-EOF
	{% for w in www.keys()|sort %}
	mkdir -p /var/www/{{ w }}

	tmpl /etc/nginx/conf.d/{{ w }}.conf \
		/etc/nginx/conf.d/site.conf www.{{ w }}

	{% if 'auth_basic' in www[w] %}
	tmpl /etc/nginx/conf.d/{{ w }}.passwd \
		/etc/nginx/conf.d/site.passwd www.{{ w }}
	{% endif %}
	{% endfor %}
	EOF

	svc nginx
fi

##
## WSGI
##

if role wsgi; then
	pkg '
		gunicorn3
		python3-setproctitle
	'

	grep -q ^gunicorn: /etc/group ||
		groupadd -r gunicorn
	grep -q ^gunicorn: /etc/passwd ||
		useradd -r -d /run/gunicorn -s /usr/sbin/nologin \
			-g gunicorn gunicorn

	file /etc/tmpfiles.d/gunicorn.conf

	mkdir -p /var/db/gunicorn
	chown gunicorn: /var/db/gunicorn

	tmplexec <<-EOF
	{% for w in wsgi.keys()|sort %}
	{% if 'venv' in wsgi[w] %}
	if ! [ -d {{ wsgi[w].venv }}/bin ]; then
		mkdir -p {{ wsgi[w].venv }}
		virtualenv -p /usr/bin/python3.7 {{ wsgi[w].venv }}
		chown -R gunicorn: {{ wsgi[w].venv }}
	fi
	{% endif %}

	tmpl /etc/systemd/system/gunicorn-{{ w }}.service \
		/etc/systemd/system/gunicorn.service wsgi.{{ w }}
	tmpl /etc/systemd/system/gunicorn-{{ w }}.socket \
		/etc/systemd/system/gunicorn.socket wsgi.{{ w }}

	svc gunicorn-{{ w }}.socket
	svc gunicorn-{{ w }}.service
	{% endfor %}
	EOF
fi

##
## DB
##

if role db; then
	pkg postgresql
	svc postgresql
fi

##
## Redis
##

if role redis; then
	pkg redis
	tmpl /etc/redis/redis.conf
	svc redis
fi

##
## Feed
##

if role feed; then
	if ! apt-key list 2>/dev/null | grep -q fred@miniflux.net; then
		curl -fsSL https://apt.miniflux.app/KEY.gpg | apt-key add -
	fi

	file /etc/apt/sources.list.d/miniflux.list

	pkg miniflux

	tmpl /etc/miniflux.conf
	chown miniflux: /etc/miniflux.conf
	chmod 640 /etc/miniflux.conf

	file /etc/tmpfiles.d/miniflux.conf

	svc miniflux


	pkg virtualenv

	if ! [ -e /opt/fluxfilter/bin/python3 ]; then
		virtualenv -p python3 /opt/fluxfilter
	fi

	pip /opt/fluxfilter 'miniflux==0.0.10'
	tmpl /opt/fluxfilter/bin/fluxfilter
	chmod +x /opt/fluxfilter/bin/fluxfilter

	mkdir -p /var/log/fluxfilter
	chown miniflux: /var/log/fluxfilter

	if ! crontab -l -u miniflux | grep -q fluxfilter; then
		(
			crontab -l -u miniflux 2>/dev/null
			echo "* * * * * /opt/fluxfilter/bin/fluxfilter  | ts \%FT\%T\%z >> /var/log/fluxfilter/fluxfilter.log"
		) | crontab -u miniflux -
	fi
fi

##
## Storage
##

if role storage; then
	pkg smartmontools
	svc smartd
fi

##
## Backup
##

if role backup; then
	pkg restic moreutils

	mkdir -p /var/log/backup /var/backups/cache

	file /etc/logrotate.d/backup

	tmplexec <<-EOF
	{% for b in backup.keys()|sort %}
	b={{ b }}
	f=/usr/local/sbin/restic-backup-\$b
	tmpl \$f /usr/local/sbin/restic-backup backup.\$b
	chmod 700 \$f

	{% if 'cron' in backup[b] %}
	if ! crontab -l | grep -q \$f; then
		(
			crontab -l 2>/dev/null
			echo "{{ backup[b].cron }} \$f"
		) | crontab -
	fi
	{% endif %}
	{% endfor %}
	EOF
fi

##
## Monitoring
##

_prominst() {
	local name=$1
	local ver=$(
		curl -s https://api.github.com/repos/prometheus/$name/releases/latest |
			jq .tag_name -j |
			sed 's/^v//'
	)
	local fname=$name-$ver.linux-amd64
	local url=https://github.com/prometheus/$name/releases/download/v$ver/$fname.tar.gz

	grep -q ^$name: /etc/group ||
		groupadd -r $name
	grep -q ^$name: /etc/passwd ||
		useradd -r -d /var/lib/$name -s /usr/sbin/nologin \
			-g $name $name
	mkdir -p /var/lib/$name
	chown $name: /var/lib/$name
	chmod 750 /var/lib/$name

	if ! $name --version 2>&1 | grep -q "version $ver"; then
		curl -L $url | tar -C /tmp/ -xz

		case "$name" in
			prometheus)
				cp \
					/tmp/$fname/prometheus \
					/tmp/$fname/promtool \
					/usr/local/bin/
				mkdir -p /etc/$name
				chown $name: /etc/$name
				chmod 750 /etc/$name
				cp -r \
					/tmp/$fname/console_libraries \
					/tmp/$fname/consoles \
					/etc/$name/
				;;
			*)
				cp /tmp/$fname/$name /usr/local/bin/
				;;
		esac
	fi

}

if role monitoring; then
	_prominst prometheus

	tmpl /etc/prometheus/prometheus.yml

	tmpl /etc/systemd/system/prometheus.service
	svc prometheus

	file /etc/apt/sources.list.d/grafana.list

	if ! apt-key list 2>/dev/null | grep -q info@grafana.com; then
		curl -fsSL https://packages.grafana.com/gpg.key | apt-key add -
	fi

	pkg grafana
	tmpl /etc/grafana/grafana.ini
	file /etc/grafana/provisioning/datasources/prometheus.yml
	svc grafana-server
fi

if role monitored; then
	pkg moreutils

	_prominst node_exporter
	tmpl /etc/systemd/system/node_exporter.service

	mkdir -p /usr/lib/node_exporter
	chown node_exporter: /usr/lib/node_exporter
	file /usr/share/node_exporter/apt
	file /etc/systemd/system/node_exporter-apt.service
	file /etc/systemd/system/node_exporter-apt.timer

	svc node_exporter-apt.timer
	svc node_exporter
fi

##
## Dyndns
##

if role dyndns; then
	pkg ddupdate python3-requests

	file /usr/share/ddupdate/plugins/cloudflare.py
	tmpl /etc/systemd/system/ddupdate.service.d/serviceopt.conf
	tmpl /etc/ddupdate.conf
	tmpl /root/.netrc

	svc ddupdate.timer
fi

##
## IRC
##

_w_run() {
	local f=/var/lib/weechat/weechat_fifo

	if ! [ -e $f ]; then
		echo Missing weechat fifo file
		exit 1
	fi

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
	pkg weechat weechat-plugins
	grep -q ^weechat: /etc/group ||
		groupadd -r weechat
	grep -q ^weechat: /etc/passwd ||
		useradd -r -d /var/lib/weechat -s /bin/sh \
			-g weechat weechat

	mkdir -p /var/lib/weechat
	chown weechat: /var/lib/weechat
	chmod 750 /var/lib/weechat
	file /etc/systemd/system/weechat.service
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
	_w_set weechat.bar.buflist.separator off
	_w_set weechat.bar.buflist.color_bg 254
	_w_set buflist.format.buffer ' ${if:${current_buffer}?${color:255}:${color:default}}${format_number}${indent}${format_nick_prefix}${if:${current_buffer}?${color:255}:${color_hotlist}}${format_name} '
	_w_set buflist.format.buffer_current '${color:255,24}${format_buffer}'
	_w_set buflist.format.hotlist_low '${color:102}'
	_w_set buflist.format.number '${number}${if:${number_displayed}? :}'

	_w_set irc.server_default.msg_part ''
	_w_set irc.server_default.msg_quit ''
	_w_set irc.server_default.sasl_mechanism plain
	_w_set irc.server_default.sasl_username $IRC_NICK
	_w_set irc.server_default.nicks $IRC_NICK

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
## Host specific system setup
##

_hostsh=$ROOT/env/$(hostname).sh

if [ -e $_hostsh ]; then
	TEMPLATES=$ROOT/env/templates
	FILES=$ROOT/env/files
	. $_hostsh
fi

##
## Cleanup
##

_UNNEEDED_PKGS='
debconf-i18n
eject
ifupdown
isc-dhcp-client
nano
rsyslog
tasksel
'

if role vm; then
	_UNNEEDED_PKGS="$_UNNEEDED_PKGS linux-image-amd64"
fi

for p in $_UNNEEDED_PKGS; do
	if _pkg_installed; then
		apt purge $p
	fi
done
unset p

for f in auth daemon kern lpr mail user syslog debug messages; do
	rm -f /var/log/$f*
done
