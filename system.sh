#!/bin/sh -e

[ "$UID" = 0 ] || exec sudo $0

ROOT=$(cd "$(dirname "$0")"; pwd -P)

. $ROOT/env
. $ROOT/lib.sh

##
## Base
##

pkg '
	pacman-contrib
	sudo
	lostfiles
'

if [ "$HEADLESS" != yes ]; then
	pkg '
		fwupd
	'
fi

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

if [ "$HEADLESS" != yes ]; then
	# Periodic TRIM:
	svc fstrim.timer
fi

file /etc/pacman.conf
# TODO: better geo loc for servers:
file /etc/pacman.d/mirrorlist
file /etc/pacman.d/hooks/needrestart.hook

if [ "$HEADLESS" != yes ]; then
	# Autologin to TTY 1:
	tmpl /etc/systemd/system/getty@tty1.service.d/override.conf '$AUTOLOGIN_USER'
fi

# Keep no pacman cache for uninstalled packages and 2 versions of
# installed packages:
file /etc/systemd/system/paccache.service.d/override.conf
svc paccache.timer

# Passwordless sudo for wheel:
echo '%wheel ALL = (ALL) NOPASSWD: ALL' > /etc/sudoers.d/wheel

##
## Net
##

if [ "$HEADLESS" != yes ]; then
	file /etc/systemd/resolved.conf.d/static.conf
fi

##
## CLI
##

pkg '
	git
	openssh
	vim
	bash-completion
	tmux
	ripgrep
	fzy
	ncdu
'

##
## Build
##

pkg '
	devtools
'

##
## Debug
##

pkg '
	ps_mem
'

##
## Dev
##

[ "$HEADLESS" = yes ] || pkg '
	make
	go
'

##
## Sec
##

pkg '
	arch-audit
	nftables
'

# TODO: enable with ssh limit for servers
if [ "$HEADLESS" != yes ]; then
	file /etc/nftables.conf
	svc nftables
fi

chmod 700 \
	/boot \
	/etc/nftables.conf

file /etc/sysctl.d/50-dmesg-restrict.conf

##
## Desktop
##

if [ "$HEADLESS" != yes ]; then
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

if [ "$HEADLESS" != yes ]; then
	file /etc/sysctl.d/disable_watchdog.conf
	file /etc/modprobe.d/audio_powersave.conf
fi

##
## Bluetooth
##

if [ "$HEADLESS" != yes ]; then
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

[ "$HEADLESS" = yes ] || pkg '
	pulseaudio
	pulsemixer
	mpv
	libva-intel-driver
'
