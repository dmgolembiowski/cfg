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
			*/xterm*|*herbst*|*/gtk*|*/mpv/*)
				continue
				;;
			*.Xresources|*.xinitrc|*xrandr*)
				continue
				;;
		esac
	fi

	if ! role irc && ! role desktop; then
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

##
## Systemd user units
##

if role desktop; then
	svc pulseaudio.socket --user
fi

##
## Vim
##

vimpack() {
	local u=$1
	local n=$2
	local v=$3
	local r=$HOME/.vim/pack/dist/start
	local fresh

	mkdir -p $r

	if [ -d $r/$n/.git ]; then
		git -C $r/$n fetch -q
	else
		fresh=yes
		echo $n: clone
		git clone -q https://github.com/$u/$n $r/$n
	fi

	local h=$(git -C $r/$n rev-parse HEAD)

	if [ "$v" ]; then
		local th=$(git -C $r/$n rev-list --tags --max-count=1)
	else
		local th=$(git -C $r/$n rev-parse origin/HEAD)
	fi

	if [ "$h" != "$th" ]; then
		echo $n
		echo '  cur:' $h
		echo '  upd:' $th

		git -C $r/$n checkout -q $th
		vim +'helptags ALL' +q

		# Skip logging if this is a fresh checkout and we're rewinding:
		if [ "$fresh" != yes ]; then
			git -C $r/$n log --pretty=oneline $h..$th | sed 's/^/  /'
		fi
	fi
}

vimpurge() {
	local n=$1
	local r=$HOME/.vim/pack/dist/start

	if [ -d $r/$n ]; then
		echo $n rm
		rm -rf $r/$n
	fi
}

if role dev; then
	# Light color scheme:
	vimpack cormacrelf vim-colors-github

	# Visualize buffers as tabs:
	vimpack ap vim-buftabline

	# Comment code (bound to gcc/gc):
	vimpack tpope vim-commentary

	# Pick files with fzy (bouund to C-p):
	vimpack srstevenson vim-picker

	# Unix shell commands (:Chmod, :SudoWrite, etc):
	vimpack tpope vim-eunuch tag

	# Create non-existant directories for new file paths:
	vimpack duggiefresh vim-easydir tag

	# Reopen files at last edit position:
	vimpack farmergreg vim-lastplace tag

	# Improved markdown syntax (folding, concealing, extensions):
	vimpack plasticboy vim-markdown

	# Syntax for jinja templates:
	vimpack  glench vim-jinja2-syntax

	# Python formatting with black:
	vimpack psf black tag

	mkdir -p ~/.vim/tmp/undo
	mkdir -p ~/.vim/tmp/swap
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
