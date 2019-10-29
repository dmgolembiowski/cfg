#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)

# Python dependencies:
for _p in PyYAML==5.1.2 Jinja2==2.10.3; do
	if ! pip3 freeze | grep -q "^$_p"; then
		tmpcache=$(mktemp -d)
		pip3 --cache-dir=$tmpcache install --user $_p
	fi
done

. $ROOT/lib.sh

# xcode development tools:
if ! [ -x /Library/Developer/CommandLineTools/usr/bin/cc ]; then
	xcode-select --install
fi

# homebrew:
if ! [ -x /usr/local/bin/brew ]; then
	hburl=https://raw.githubusercontent.com/Homebrew/install/master/install
	ruby -e "$(curl -fsSL $hburl)"
fi

brew bundle

# Modern bash as default shell:
_s=/usr/local/bin/bash
if ! grep -q $_s /etc/shells; then
	sudo sh -c "echo $_s >> /etc/shells"
fi
if [ "$SHELL" != "$_s" ]; then
	chsh -s $_s
fi

# SF Mono font:
if ! [ -e ~/Library/Fonts/SFMono-Regular.otf ]; then
	cp /System/Applications/Utilities/Terminal.app/Contents/Resources/Fonts/*.otf \
		~/Library/Fonts
fi

# SSH tunnel:

tmpl ~/Library/LaunchAgents/ssh-tunnel.plist \
	/home/user/Library/LaunchAgents/ssh-tunnel.plist

if ! launchctl list | grep -q 'ssh-tunnel$'; then
	launchctl load ~/Library/LaunchAgents/ssh-tunnel.plist
fi
