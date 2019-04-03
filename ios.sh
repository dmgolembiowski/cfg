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

##
## CLI
##

pkg '
	vim
	bash
	bash-completion
'
