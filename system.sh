#!/bin/sh -e

[ "$(id -u)" = 0 ] || exec sudo $0 $(id -un )

UNPRIVUSR="$1"
shift

ROOT=$(cd "$(dirname "$0")"; pwd -P)

. $ROOT/lib.sh
. $ROOT/ver.sh

##
## Base
##

pkg '
	sudo
	curl
	ca-certificates
	'

pkg '
	python3-jinja2
	python3-yaml
	unattended-upgrades
	python3-gi
	needrestart
	'
echo '%sudo ALL = (ALL) NOPASSWD: ALL' > /etc/sudoers.d/sudo-nopasswd

tmpl /etc/apt/sources.list

for f in norecommends autoremove periodicclean showversions; do
	file /etc/apt/apt.conf.d/$f
done
unset f

file /usr/local/bin/apt-backports
chmod +x /usr/local/bin/apt-backports

pkg openssh-client
if role server; then
	pkg ssh
fi

if role vm; then
	pkg linux-image-cloud-amd64
else
	pkg fwupd policykit-1 tpm2-tools intel-microcode
fi

# Persistend systemd yournal:
mkdir -p /var/log/journal

svc systemd-timesyncd

if role desktop; then
	_blank_cmd='consoleblank=60'
	if ! grep -q $_blank_cmd /etc/default/grub; then
		sed -i "s/^\(GRUB_CMDLINE_LINUX_DEF.*\)\"/\1 $_blank_cmd\"/" \
			/etc/default/grub
		update-grub
	fi
fi

##
## Net
##

file /etc/systemd/resolved.conf.d/dns_servers.conf

if role desktop; then
	file /etc/systemd/resolved.conf.d/static.conf

	if ! role vm; then
		_wlif=$(ip a | awk '/^[0-9]: wl/ { print $2 }' | tr -d :)
		envfile /etc/wpa_supplicant/wpa_supplicant-${_wlif}.conf

		pkg '
			iw
			wavemon
			'
	fi
fi

tmpl /etc/systemd/network/wired.network
svc systemd-networkd

svc systemd-resolved
ln -sf /run/systemd/resolve/resolv.conf /etc/resolv.conf

##
## Sec
##

pkg nftables

tmpl /etc/nftables.conf
chmod 700 /etc/nftables.conf

svc nftables

chmod 700 /boot

file /etc/sysctl.d/50-dmesg-restrict.conf

pkg debian-security-support

##
## Debug
##

_psv=3.13
u=https://raw.githubusercontent.com/pixelb/ps_mem/v$_psv/ps_mem.py
b=/usr/local/bin/ps_mem
if ! grep -q "^# V$_psv" $b 2>/dev/null; then
	curl -L $u > $b
	sed -i 's/env python/env python3/' $b
	chmod +x $b
fi

pkg htop

##
## Dev
##

if role dev; then
	pkg '
		vim-nox
		fzy
		man-db
		ncurses-term
		silversearcher-ag
		git
		bash-completion
		tmux
		ncdu
		jq
		'
fi

if role work; then
	if ! [ -e /opt/az/bin/python3 ]; then
		python3 -m venv /opt/az
	fi

	pip /opt/az 'azure-cli==2.1.0'
	file /usr/local/bin/az
	chmod +x /usr/local/bin/az
	file /usr/share/bash-completion/completions/az

	file /etc/apt/sources.list.d/slack.list
	pkg slack-desktop
fi

##
## Pkg
##

if role pkg; then
	pkg '
		build-essential
		devscripts
		equivs
		git-buildpackage
		pristine-tar
		debspawn
		lintian
		debmake
		python3-debian
		licensecheck
		'
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
		herbstluftwm
		lemonbar-xft
		rofi
		inotify-tools
		bc
		xterm
		unclutter-xfixes
		brightnessctl
		brightness-udev
		fonts-noto-core
		fonts-noto-mono
		fonts-noto-color-emoji
		fonts-ibm-plex
		xdg-utils
		physlock
		redshift
		xclip
		maim
		sxiv
		mupdf
		unzip
		bzip2
		libdbus-glib-1-2
		moreutils
		'

	svc fstrim.timer

	# Autologin to TTY 1:
	tmpl /etc/systemd/system/getty@tty1.service.d/override.conf

	file /etc/sysctl.d/disable_watchdog.conf
	file /etc/modprobe.d/audio_powersave.conf

	file /etc/modprobe.d/hid_apple.conf

	file /etc/X11/xorg.conf.d/00-keyboard.conf
	file /etc/X11/xorg.conf.d/00-touchpad.conf
	file /etc/X11/xorg.conf.d/00-pointer.conf

	# Need to reset xkbmap after resume since external keyboard is
	# connected through the screen which goes to sleep:
	file /lib/systemd/system-sleep/setxkbmap
	chmod +x /lib/systemd/system-sleep/setxkbmap

	pkg '
		bluez
		rfkill
		'
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
		'
fi

if role desktop || role media; then
	pkg lftp
fi

##
## Mail
##

if role server; then
	pkg '
		nullmailer
		bsd-mailx
		'

	tmpl /etc/nullmailer/remotes
	tmpl /etc/nullmailer/adminaddr

	svc nullmailer
fi

##
## TLS
##

if role tls; then
	pkg '
		dehydrated
		jq
		moreutils
		'

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
		python3 -m venv {{ wsgi[w].venv }}
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

	if ! [ -e /opt/fluxfilter/bin/python3 ]; then
		python3 -m venv /opt/fluxfilter
	fi

	pip /opt/fluxfilter 'miniflux==0.0.10'
	tmpl /opt/fluxfilter/bin/fluxfilter
	chmod +x /opt/fluxfilter/bin/fluxfilter

	mkdir -p /var/log/fluxfilter
	chown miniflux: /var/log/fluxfilter

	if ! crontab -l -u miniflux | grep -q fluxfilter; then
		(
			crontab -l -u miniflux 2>/dev/null
			echo "* * * * * /opt/fluxfilter/bin/fluxfilter	| ts \%FT\%T\%z >> /var/log/fluxfilter/fluxfilter.log"
		) | crontab -u miniflux -
	fi
fi

##
## Repo
##

if role repo; then
	pkg reprepro

	grep -q ^repo: /etc/group ||
		groupadd -r repo
	grep -q ^repo: /etc/passwd ||
		useradd -r -d /var/lib/repo -s /bin/bash \
				-g repo repo

	tmpl /var/lib/repo/.ssh/authorized_keys
	tmpl /var/lib/repo/conf/distributions
	tmpl /var/lib/repo/conf/options
	tmpl /var/lib/repo/conf/incoming

	mkdir -p /var/lib/repo/incoming
	mkdir -p /var/lib/repo/pub

	chown -R repo: /var/lib/repo
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
	pkg '
		restic
		moreutils
		'

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
	local ver=$2
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
			alertmanager)
				cp \
					/tmp/$fname/alertmanager \
					/tmp/$fname/amtool \
					/usr/local/bin/
				;;
			*)
				cp /tmp/$fname/$name /usr/local/bin/
				;;
		esac
	fi

}

if role monitoring; then
	_prominst prometheus $PROMETHEUS_V
	tmpl /etc/prometheus/prometheus.yml
	file /etc/prometheus/rules.d/node.rules
	tmpl /etc/systemd/system/prometheus.service
	svc prometheus

	_prominst alertmanager $ALERTMANAGER_V
	tmpl /etc/alertmanager/alertmanager.yml
	tmpl /etc/systemd/system/alertmanager.service
	svc alertmanager

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

	_prominst node_exporter $NODE_EXPORTER_V
	tmpl /etc/systemd/system/node_exporter.service

	mkdir -p /usr/lib/node_exporter
	chown node_exporter: /usr/lib/node_exporter

	for f in apt needrestart; do
		file /usr/share/node_exporter/$f
		chmod +x /usr/share/node_exporter/$f
		file /etc/systemd/system/node_exporter-$f.service
		file /etc/systemd/system/node_exporter-$f.timer
		svc node_exporter-$f.timer
	done
	svc node_exporter
fi

##
## Dyndns
##

if role dyndns; then
	pkg '
		ddupdate
		python3-requests
		'

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

	case  "$set_key" in
		*nicks|*realname|*sasl_username|weechat.bar.buflist.items)
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
	pkg '
		weechat
		weechat-plugins
		'
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

	_w_set weechat.look.buffer_time_format '%H:%M'
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
	_w_set buflist.format.number '${number}${if:${number_displayed}? :}'

	_w_set irc.server_default.msg_part ''
	_w_set irc.server_default.msg_quit ''
	_w_set irc.server_default.sasl_mechanism plain
	_w_set irc.server_default.sasl_username $IRC_NICK
	_w_set irc.server_default.nicks $IRC_NICK

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
	if _pkg_installed $p; then
		apt purge $p
	fi
done
unset p

for f in auth daemon kern lpr mail user syslog debug messages; do
	rm -f /var/log/$f*
done
