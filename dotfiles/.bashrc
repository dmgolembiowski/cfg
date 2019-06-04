#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ $WAYLAND_DISPLAY ]] && shopt -s checkwinsize

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

case ${TERM} in
  xterm*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033]0;%s\007" "${PWD/#$HOME/\~}"'

    ;;
  screen*)
    PROMPT_COMMAND=${PROMPT_COMMAND:+$PROMPT_COMMAND; }'printf "\033_%s\033\\" "${PWD/#$HOME/\~}"'
    ;;
esac

MANWIDTH=80
export MANWIDTH

PAGER=less
LESS=-iFXR
EDITOR=vi
export PAGER LESS EDITOR

case $PATH in
*$HOME/.local/bin*)
	:
	;;
*)
	PATH=$HOME/.local/bin:$PATH
	;;
esac

if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
fi

command -v vim >/dev/null && EDITOR=vim

alias ls='ls -1F'

alias g=git
alias ga='git add -p'
alias gc='git ci'
alias gs='git st'
alias gd='git diff'
alias gb='git branch'
alias gp='git push'
alias gl='git pull'

GOPATH=$HOME/src/go
export GOPATH

for _f in $GOPATH /opt/go; do
	if [ -d $_f/bin ]; then
		case $PATH in
		*$_f/bin*)
			:
			;;
		*)
			PATH=$PATH:$_f/bin
			;;
		esac
	fi
done
unset _f

if [ -z "$WAYLAND_DISPLAY" ] && [ "$(tty)" = "/dev/tty1" ]; then
	exec startx
fi
