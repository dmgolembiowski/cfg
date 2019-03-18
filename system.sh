#!/bin/sh -e

p() {
	for p; do
		pacman -Q $p >/dev/null 2>&1 || pacman -S $p
	done
}

##
## CLI
##

p git openssh vim

##
## Desktop
##

p sway alacritty firefox
