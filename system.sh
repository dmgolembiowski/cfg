#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)
FILES=$ROOT/files
TEMPLATES=$ROOT/templates

. $ROOT/env

diff() {
	if command -v git >/dev/null; then
		git diff --no-index "$@"
	else
		comamnd diff -u "$@"
	fi
}

pkg() {
	for p; do
		pacman -Q $p >/dev/null 2>&1 || pacman -S $p
	done
}

_f() {
	local dst=$1
	local src=$2
	mkdir -p $(dirname $dst)
	if [ -e $dst ]; then
		diff $dst $src || :
	fi
	cp $src $dst
}

file() {
	_f $1 $FILES$1
}

tmpl() {
	local f=$1
	shift
	local tmp=/tmp/$(echo $f | sed 's#/#_#g')
	envsubst "$@" < $TEMPLATES$f > $tmp
	_f $f $tmp
}

##
## CLI
##

pkg git openssh vim

##
## Desktop
##

pkg sway xorg-server-xwayland alacritty firefox

# Autologin to TTY 1:
tmpl /etc/systemd/system/getty@tty1.service.d/override.conf '$AUTOLOGIN_USER'
