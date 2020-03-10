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

##
## Dirs
##

if role desktop; then
    mkdir -p ~/pic
fi
