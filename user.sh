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

	# TODO: port to xterm/i3:
	case "$f" in
		*/alacritty*|/systemd*|/sway*)
			continue
			;;
	esac

	if role server; then
		case "$f" in
			*/systemd*|*/sway*|*/alacritty*|*/gtk*|*/i3*|*/imv/*)
				continue
				;;
			*/mpv/*|*firefox*|*pacsize|*spotify*|*pam_env*)
				continue
				;;
			*/mako/*|*/slack|*/plex*|*/spotify)
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

if role desktop; then
	svc ssh-agent --user
	tmpl ~/.config/systemd/user/ssh-tunnel.service \
		/home/user/.config/systemd/user/ssh-tunnel.service
	svc ssh-tunnel --user
fi

##
## Desktop
##

# TODO: fix for i3
# if role desktop; then
# 	systemctl --user enable swayidle systemctl --user start swayidle
# 	systemctl --user enable mako
# 	systemctl --user start mako
# fi

##
## Firefox
##

if role desktop; then
	ff_profile_dir=~/.mozilla/firefox

	for d in $ff_profile_dir/*.default-release $ff_profile_dir/*.priv; do
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

		git -C $r/$n checkout $th
		vim +'helptags ALL' +q

		# Skip logging if this is a fresh checkout and we're rewinding:
		if [ "$fresh" != yes ]; then
			git log --pretty=oneline $h..$th | sed 's/^/  /'
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
	vimpack NLKNguyen papercolor-theme
	vimpack ap vim-buftabline
	vimpack tpope vim-commentary
	vimpack srstevenson vim-picker
	vimpack tpope vim-eunuch tag
	vimpack duggiefresh vim-easydir tag
	vimpack farmergreg vim-lastplace tag
	vimpack vimwiki vimwiki tag
	if role work; then
		vimpack fatih vim-go tag
	fi

	vimpurge nofrils

	mkdir -p ~/.vim/tmp/undo
	mkdir -p ~/.vim/tmp/swap
fi

##
## Dirs
##

if role desktop; then
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
