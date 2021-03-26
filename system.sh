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
tmpl /etc/apt/auth.conf

if [ "$DEBIAN_SRC" = sid ]; then
	file /etc/apt/preferences.d/99-sid
fi

for f in norecommends autoremove periodicclean showversions; do
	file /etc/apt/apt.conf.d/$f
done
unset f

file /usr/local/bin/apt-backports
chmod +x /usr/local/bin/apt-backports
file /usr/local/bin/apt-size
chmod +x /usr/local/bin/apt-size

pkg openssh-client
if role server; then
	pkg ssh
fi

if role vm; then
	pkg linux-image-cloud-amd64
fi

# Persistend systemd yournal:
mkdir -p /var/log/journal

svc systemd-timesyncd

##
## Net
##

file /etc/systemd/resolved.conf.d/dns_servers.conf

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

u=https://raw.githubusercontent.com/pixelb/ps_mem/v$PSMEM_V/ps_mem.py
b=/usr/local/bin/ps_mem
if ! grep -q "^# V$PSMEM_V" $b 2>/dev/null; then
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
	# TODO: manual installed https://github.com/miniflux/v2/releases/download/2.0.26/miniflux_2.0.26_amd64.deb

	tmpl /etc/miniflux.conf
	chown miniflux: /etc/miniflux.conf
	chmod 640 /etc/miniflux.conf

	file /etc/tmpfiles.d/miniflux.conf

	svc miniflux
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

	_prominst node_exporter $NODEEXPORTER_V
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
