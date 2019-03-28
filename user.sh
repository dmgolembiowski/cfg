#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)
DOTFILES=$ROOT/dotfiles

. $ROOT/env
. $ROOT/lib.sh

##
## Dotfiles
##

find $DOTFILES -type f -print0 | while IFS= read -r -d '' f; do
	rel=${f##$DOTFILES/}
	src="$DOTFILES/$rel"
	dst="$HOME/$rel"

	mkdir -p "$(dirname "$dst")"

	if ! [ -L "$dst" ]; then
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
## Taskwarrior
##

tmpl ~/.taskrc \
	/home/user/.taskrc \
	'$TASKD_CREDENTIALS'

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

##
## Todoist
##

tmpl ~/.todoist.config.json \
	/home/user/.todoist.config.json \
	'$TODOIST_API_TOKEN'
