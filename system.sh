#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)
FILES=$ROOT/files

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

f() {
	local dst=$1
	local src=$FILES$1
	mkdir -p $(dirname $dst)
	if [ -e $dst ]; then
		diff $dst $src
	fi
	cp $src $dst
}

##
## CLI
##

pkg git openssh vim

##
## Desktop
##

pkg sway xorg-server-xwayland alacritty firefox
