#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ $WAYLAND_DISPLAY ]] && shopt -s checkwinsize

# Completions on MacOS:
if [ -r /usr/local/etc/profile.d/bash_completion.sh ]; then
	export BASH_COMPLETION_COMPAT_DIR=/usr/local/etc/bash_completion.d
	. /usr/local/etc//profile.d/bash_completion.sh
fi

shopt -s histappend
shopt -s extglob
shopt -s globstar

HISTSIZE=10000
HISTFILESIZE=$HISTSIZE
HISTCONTROL=ignoreboth

unset MAILCHECK

if [[ ${EUID} == 0 ]]; then
	PS1='\[\033[00;31m\]\w\[\033[0m\] '
else
	PS1='\w '
fi

if [ "$TERM" = dumb ]; then
	PS1='> '
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

LANG=en_US.UTF-8
LC_ALL=$LANG
export LC_ALL LANG

PAGER=less
LESS=-iFXR
EDITOR=vi
if command -v vim >/dev/null; then
	EDITOR=vim
fi
export PAGER LESS EDITOR

case $PATH in
*$HOME/.local/bin*)
	:
	;;
*)
	PATH=$HOME/.local/bin:$PATH
	;;
esac

if [ "$TERM" = xterm ]; then
	TERM=xterm-256color
fi

if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
fi

alias ls='ls -1F'

alias g=git
alias ga='git add -p'
alias gc='git ci'
alias gs='git st'
alias gd='git diff'
alias gb='git branch'
alias gp='git push'
alias gl='git pull'

alias df='df -x squashfs -x tmpfs -x devtmpfs -x fuse.rar2fs'

alias xcp='xclip -selection clipboard'


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

if [ "$(tty)" = "/dev/tty1" ]; then
	exec startx
fi

# Check for restarts:
if [ -e /var/run/reboot-required ]; then
	printf '##\n## Reboot required\n##\n'
fi
