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

	case "$f" in
		'.#'*|*'~')
			continue
			;;
	esac

	if role server; then
		case "$f" in
			*/xterm*|*/gtk*|*/mpv/*)
				continue
				;;
			*.Xresources|*.xinitrc|*xrandr*)
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

	mkdir -p "$(dirname "$dst")"

	if [ ! -L "$dst" -o "$(readlink $dst)" != "$src" ]; then
		printf '%s -> %s\n'  "$src" "$dst"
		ln -sf "$src" "$dst"
	fi
done

if ! [ -L $HOME/.bash_profile ]; then
	ln -sf $HOME/.bashrc $HOME/.bash_profile
fi

if role desktop; then
	svc pulseaudio.socket --user
fi

if role desktop; then
	_ffu='https://download.mozilla.org/'
	_ffu="$_ffu?product=firefox-latest&os=linux64&lang=en-US"
	_opt=~/.local/opt
	_bin=~/.local/bin

	if ! [ -e $_opt/firefox/firefox ]; then
		mkdir -p $_opt
		curl -L $_ffu | tar -C $_opt -xj
	fi

	if ! [ -e $_bin/firefox ]; then
		ln -s $_opt/firefox/firefox $_bin/firefox
	fi
fi

##
## Dirs
##

if role desktop; then
	mkdir -p ~/pic
fi
