#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)

for _p in PyYAML==5.1.2 ; do
	if ! pip3 freeze | grep -q "^$_p"; then
		tmpcache=$(mktemp -d)
		pip3 --cache-dir=$tmpcache install --user $_p
	fi
done

. $ROOT/lib.sh

if ! [ -x /Library/Developer/CommandLineTools/usr/bin/cc ]; then
	xcode-select --install
fi

if ! [ -x /usr/local/bin/brew ]; then
	hburl=https://raw.githubusercontent.com/Homebrew/install/master/install
	ruby -e "$(curl -fsSL $hburl)"
fi

brew bundle

_s=/usr/local/bin/bash
if ! grep -q $_s /etc/shells; then
	sudo sh -c "echo $_s >> /etc/shells"
fi
if [ "$SHELL" != "$_s" ]; then
	chsh -s $_s
fi
