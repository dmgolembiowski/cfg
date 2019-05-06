#!/bin/sh -e

[ "$(id -u)" = 0 ] || exec sudo $0

ROOT=$(cd "$(dirname "$0")"; pwd -P)

. $ROOT/env
. $ROOT/lib.sh

##
## Base
##

if distro alpine; then
	pkg gettext  # for envsubst
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
	tmpl /etc/systemd/system/getty@tty1.service.d/override.conf '$AUTOLOGIN_USER'
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

# TODO: enable with ssh limit for servers
if role desktop; then
	file /etc/nftables.conf
	svc nftables
	chmod 700 /etc/nftables.conf
fi

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

if role bouncer; then
	pkg znc znc-extra znc-playback
	if [ -e /var/lib/znc/configs/znc.conf ]; then
		svc znc
	else
		echo Run /etc/init.d/znc setup
	fi
fi

##
## Mailserver
##

if role mailsrv; then
	pkg postfix dovecot dovecot-lmtpd

	tmpl /etc/postfix/main.cf '$MAIL_DOMAIN'
	tmpl /etc/postfix/aliases '$MAIL_OWNER'

	a=/etc/postfix/aliases
	if [ ! -e $a.db -o $a -nt $a.db ]; then
		echo Generating new $a.db
		newaliases
	fi
	unset a

	svc postfix

	tmpl /etc/dovecot/conf.d/00-local.conf

	svc dovecot
fi
