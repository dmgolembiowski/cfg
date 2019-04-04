#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)
DOTFILES=$ROOT/dotfiles

. $ROOT/env
. $ROOT/lib.sh

##
## Base
##

pkg '
	gettext
'

export ALPINE_V=v3.9
tmpl /etc/apk/repositories '$ALPINE_V'

##
## CLI
##

pkg '
	vim
	git-perl
	less
	bash
	bash-completion
	fzy@testing
'

# TODO: change shell to bash
