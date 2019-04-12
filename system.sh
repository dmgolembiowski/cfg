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
	fwupd
	lostfiles
'

# Unneeded base packages:
UNNEEDED_PKGS='
	dhcpcd
	jfsutils
	linux
	lvm2
	mdadm
	nano
	netctl
	psmisc
	reiserfsprogs
	s-nail
	systemd-sysvcompat
	xfsprogs
'

for p in $UNNEEDED_PKGS; do
	if pacman -Q $p >/dev/null 2>&1; then
		pacman -Rs $p
	fi
done
unset p

# Periodic TRIM:
svc fstrim.timer

file /etc/pacman.conf
file /etc/pacman.d/mirrorlist
file /etc/pacman.d/hooks/needrestart.hook

# Autologin to TTY 1:
tmpl /etc/systemd/system/getty@tty1.service.d/override.conf '$AUTOLOGIN_USER'

# Keep no pacman cache for uninstalled packages and 2 versions of
# installed packages:
file /etc/systemd/system/paccache.service.d/override.conf
svc paccache.timer

# Passwordless sudo for wheel:
echo '%wheel ALL = (ALL) NOPASSWD: ALL' > /etc/sudoers.d/wheel

##
## Net
##

file /etc/systemd/resolved.conf.d/static.conf

##
## CLI
##

pkg '
	git
	openssh
	vim
	bash-completion
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

pkg '
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

file /etc/nftables.conf
svc nftables

chmod 700 \
	/boot \
	/etc/nftables.conf

file /etc/sysctl.d/50-dmesg-restrict.conf

##
## Desktop
##

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

##
## Laptop
##

file /etc/sysctl.d/disable_watchdog.conf
file /etc/modprobe.d/audio_powersave.conf

##
## Bluetooth
##

pkg '
	bluez
	bluez-utils
'

file /etc/bluetooth/main.conf

svc bluetooth


##
## Media
##

pkg '
	pulseaudio
	pulsemixer
	mpv
	libva-intel-driver
'
