#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)
DOTFILES=$ROOT/dotfiles

. $ROOT/lib.sh

##
## Dotfiles
##

find $DOTFILES -type f | while read -r f; do
	rel=${f##$DOTFILES/}
	src="$DOTFILES/$rel"
	dst="$HOME/$rel"


	if role server || mac; then
		case "$f" in
			*/systemd*|*/xterm*|*/gtk*|*/mpv/*)
				continue
				;;
			*pam_env*|*.emacs.d*|*org-sync*)
				continue
				;;
			*.Xresources|*.xinitrc)
				continue
				;;
			*bin/pac*)
				continue
				;;
		esac
	fi

	if ! role irc; then
		case "$f" in
			*bin/irc)
				continue
				;;
		esac
	fi

	if ! mac; then
		case "$f" in
			*hammerspoon*|*alacritty*)
				continue
				;;
		esac
	fi

	mkdir -p "$(dirname "$dst")"

	if [ ! -L "$dst" -o "$(readlink $dst)" != "$src" ]; then
		printf '%s -> %s\n'  "$src" "$dst"
		ln -sf "$src" "$dst"
	fi
done

if ! [ -L $HOME/.bash_profile ]; then
	ln -sf $HOME/.bashrc $HOME/.bash_profile
fi

##
## Systemd user units
##

if role desktop && ! mac; then
	systemctl --user enable redshift
fi

##
## Dirs
##

if role desktop && ! mac; then
	mkdir -p ~/pic
fi
