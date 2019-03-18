#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

if [ "$(tty)" = "/dev/tty1" ]; then
	exec sway
fi

[[ $DISPLAY ]] && shopt -s checkwinsize

shopt -s histappend
shopt -s extglob
shopt -s globstar

HISTSIZE=10000
HISTFILESIZE=$HISTSIZE
HISTCONTROL=ignoreboth

unset MAILCHECK

set -o vi

if [[ ${EUID} == 0 ]]; then
	PS1='\[\033[00;31m\]\w\[\033[0m\] '
else
	PS1='\w '
fi

PAGER=less
LESS=-iFXR
EDITOR=vi
export PAGER LESS EDITOR

case $PATH in
*$HOME/bin*)
	:
	;;
*)
	PATH=$HOME/bin:$PATH
	;;
esac

command -v vim >/dev/null && EDITOR=vim

alias ls='ls -1F'

GOPATH=$HOME/src/go
export GOPATH

if [ -d $GOPATH/bin ]; then
	PATH=$PATH:$GOPATH/bin
fi
