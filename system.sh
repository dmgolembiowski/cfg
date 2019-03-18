#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)

. $ROOT/env
. $ROOT/lib.sh

##
## CLI
##

pkg git openssh vim ps_mem bash-completion

##
## Dev
##

pkg go

##
## Desktop
##

pkg sway xorg-server-xwayland alacritty firefox

# Autologin to TTY 1:
tmpl /etc/systemd/system/getty@tty1.service.d/override.conf '$AUTOLOGIN_USER'
