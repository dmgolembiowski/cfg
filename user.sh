#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)
DOTFILES=$ROOT/dotfiles

. $ROOT/env
. $ROOT/lib.sh

##
## Dotfiles
##

find $DOTFILES -type f | while read -r f; do
	rel=${f##$DOTFILES/}
	src="$DOTFILES/$rel"
	dst="$HOME/$rel"

	mkdir -p "$(dirname "$dst")"

	if ! [ -L "$dst" ]; then
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

if [ "$HEADLESS" != yes ]; then
	svc ssh-agent --user
	tmpl ~/.config/systemd/user/ssh-tunnel.service \
		/home/user/.config/systemd/user/ssh-tunnel.service \
		'$SSH_TUNNEL_HOST'
	svc ssh-tunnel --user
fi

##
## Firefox
##

if [ "$HEADLESS" != yes ]; then
	ff_profile_dir=~/.mozilla/firefox

	for d in $ff_profile_dir/*.default $ff_profile_dir/*.priv; do
		[ -d "$d" ] || continue

		tmpl "$d"/chrome/userChrome.css \
			/home/user/.mozilla/firefox/profile/chrome/userChrome.css
	done
fi

##
## Vim
##

vimpack() {
	local u=$1
	local n=$2
	local v=$3
	local r=$HOME/.vim/pack/dist/start

	mkdir -p $r

	if [ -d $r/$n/.git ]; then
		git -C $r/$n fetch
	else
		git clone https://github.com/$u/$n $r/$n
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

		git -C $r/$n checkout $th
		vim +'helptags ALL' +q
	fi
}

vimpurge() {
	local n=$1
	local r=$HOME/.vim/pack/dist/start

	echo $n rm
	rm -rf $r/$n
}

vimpack robertmeta nofrils
vimpack ap vim-buftabline
vimpack tpope vim-commentary
vimpack srstevenson vim-picker
vimpack tpope vim-eunuch tag
vimpack duggiefresh vim-easydir tag
vimpack farmergreg vim-lastplace tag
vimpack vimwiki vimwiki tag
if [ "$HEADLESS" != yes ]; then
	vimpack fatih vim-go tag
fi

vimpurge ctrlp.vim

##
## Dirs
##

if [ "$HEADLESS" != yes ]; then
	mkdir -p ~/pic
fi
