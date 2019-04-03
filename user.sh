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

##
## SSH
##

svc ssh-agent --user
tmpl ~/.config/systemd/user/ssh-tunnel.service \
	/home/user/.config/systemd/user/ssh-tunnel.service \
	'$SSH_TUNNEL_HOST'
svc ssh-tunnel --user

##
## Firefox
##

ff_profile_dir=~/.mozilla/firefox

for d in $ff_profile_dir/*.default $ff_profile_dir/*.priv; do
	[ -d "$d" ] || continue

	tmpl "$d"/chrome/userChrome.css \
		/home/user/.mozilla/firefox/profile/chrome/userChrome.css
done

##
## Vim
##

f=~/.vim/autoload/plug.vim
[ -e $f ] ||
	curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
		https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
unset f

##
## Dirs
##

mkdir -p ~/pic
