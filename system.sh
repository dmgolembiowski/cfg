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

# Autologin to TTY 1:
tmpl /etc/systemd/system/getty@tty1.service.d/override.conf '$AUTOLOGIN_USER'

# Passwordless sudo for wheel:
echo '%wheel ALL = (ALL) NOPASSWD: ALL' > /etc/sudoers.d/wheel
