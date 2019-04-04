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

tmpl /etc/apk/repositories

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
