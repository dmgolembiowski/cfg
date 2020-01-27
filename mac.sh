#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)

##
## Dependencies:
##

# Python libraries:
for _p in PyYAML==5.1.2 Jinja2==2.10.3; do
	if ! pip3 freeze | grep -q "^$_p"; then
		tmpcache=$(mktemp -d)
		pip3 --cache-dir=$tmpcache install --user $_p
	fi
done

. $ROOT/lib.sh

# Xcode development tools:
if ! [ -x /Library/Developer/CommandLineTools/usr/bin/cc ]; then
	xcode-select --install
fi

# Homebrew:
if ! [ -x /usr/local/bin/brew ]; then
	hburl=https://raw.githubusercontent.com/Homebrew/install/master/install
	ruby -e "$(curl -fsSL $hburl)"
fi

# Emacs:

_ed=/Applications/Emacs.app/Contents/MacOS
if command file $_ed/Emacs | grep -q Ruby; then
	mv $_ed/Emacs{,-launcher}
	mv $_ed/Emacs{-x86_64-10_14,}
fi

exit 0

##
## Apps (see Brewfile):
##

brew bundle

##
## Configuration:
##

# Modern bash as default shell:
_s=/usr/local/bin/bash
if ! grep -q $_s /etc/shells; then
	sudo sh -c "echo $_s >> /etc/shells"
fi
if [ "$SHELL" != "$_s" ]; then
	chsh -s $_s
fi

# Make SF Mono font available system wide:
if ! [ -e ~/Library/Fonts/SFMono-Regular.otf ]; then
	cp /System/Applications/Utilities/Terminal.app/Contents/Resources/Fonts/*.otf \
		~/Library/Fonts
fi

##
## Settings:
##

# Disable resume of windows after reboot:
defaults write com.apple.systempreferences NSQuitAlwaysKeepsWindows -bool false

# Disable automatic termination of inactive apps:
defaults write NSGlobalDomain NSDisableAutomaticTermination -bool true

# Allow quitting Finder (will also hide desktop icons):
defaults write com.apple.finder QuitMenuItem -bool true

# Disable the warning before emptying the Trash
defaults write com.apple.finder WarnOnEmptyTrash -bool false

# Allow all elements to be focusable with Tab key:
defaults write NSGlobalDomain AppleKeyboardUIMode -int 3

ac() {
	pmset -g custom | sed -n '/^AC/,$p' | awk "/ $1 / { print \$2 }"
}

bat() {
	pmset -g custom | sed '/^AC/q' | awk "/ $1 / { print \$2 }"
}

# Disable machine sleep while charging
if [ "$(ac sleep)" -ne 0 ]; then
	sudo pmset -c sleep 0
fi

# Set machine sleep to 5 minutes on battery
if [ "$(bat sleep)" -ne 5 ]; then
	sudo pmset -b sleep 5
fi

# MAS: automatic update check:
defaults write com.apple.SoftwareUpdate AutomaticCheckEnabled -bool true

# MAS: check for udates daily:
defaults write com.apple.SoftwareUpdate ScheduleFrequency -int 1

# MAS: download updates automatically:
defaults write com.apple.SoftwareUpdate AutomaticDownload -int 1

# MAS: install system/sec updates automatically:
defaults write com.apple.SoftwareUpdate CriticalUpdateInstall -int 1

# MAS: install udpates automatically:
defaults write com.apple.commerce AutoUpdate -bool true

# Disalbe default and signed downloaded apps from opening up the firewall:
_sf=/usr/libexec/ApplicationFirewall/socketfilterfw
if $_sf --getallowsigned | grep -q ENABLED; then
	$_sf --setallowsigned off
	$_sf --setallowsignedapp off
fi

##
## Host specific setup
##

_hostsh=$ROOT/env/$(hostname -s).sh

if [ -e $_hostsh ]; then
	TEMPLATES=$ROOT/env/templates
	FILES=$ROOT/env/files
	. $_hostsh
fi
