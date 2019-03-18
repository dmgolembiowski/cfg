#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)
DOTFILES=$ROOT/dotfiles

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

