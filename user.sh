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


	if role server || role vm || mac; then
		case "$f" in
			*spotify*|*/slack|*/plex*)
				continue
				;;
		esac
	fi

	if role server || mac; then
		case "$f" in
			*/systemd*|*/xterm*|*/gtk*|*/i3*|*/mpv/*)
				continue
				;;
			*pam_env*)
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
## SSH
##

if role desktop && ! mac; then
	tmpl ~/.config/systemd/user/ssh-tunnel.service \
		/home/user/.config/systemd/user/ssh-tunnel.service
	svc ssh-tunnel --user

	systemctl --user enable redshift
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
	vimpack lifepillar vim-gruvbox8
	vimpack ap vim-buftabline
	vimpack tpope vim-commentary
	vimpack srstevenson vim-picker
	vimpack scrooloose nerdtree
	vimpack tpope vim-eunuch tag
	vimpack duggiefresh vim-easydir tag
	vimpack farmergreg vim-lastplace tag
	vimpack godlygeek tabular
	vimpack plasticboy vim-markdown
	if role work; then
		vimpack fatih vim-go tag
	fi

	vimpurge nofrils
	vimpurge papercolor-theme
	vimpurge vimwiki

	mkdir -p ~/.vim/tmp/undo
	mkdir -p ~/.vim/tmp/swap
fi

##
## Dirs
##

if role desktop && ! mac; then
	mkdir -p ~/pic
fi

##
## Mail
##

if role mail; then
	tmpl ~/.sieve/default.sieve /home/user/.sieve/default.sieve
	if ! [ -L ~/.dovecot.sieve ]; then
		ln -s ~/.sieve/default.sieve ~/.dovecot.sieve
	fi
fi
