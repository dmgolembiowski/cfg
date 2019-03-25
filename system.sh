#!/bin/sh -e

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
	fzf
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
	 yubikey-manager
'

svc pcscd.socket

##
## Desktop
##

pkg '
	sway
	swaylock
	swayidle
	grim
	slurp
	xorg-server-xwayland
	i3status
	alacritty
	firefox
	imv
	ttf-ibm-plex
'

##
## Media
##

pkg '
	pulseaudio
	pulsemixer
	mpv
	libva-intel-driver
'
