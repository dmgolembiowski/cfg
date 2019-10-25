#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)

. $ROOT/lib.sh

brew bundle

_s=/usr/local/bin/bash
if ! grep -q $_s /etc/shells; then
	sudo sh -c "echo $_s >> /etc/shells"
fi
if [ "$SHELL" != "$_s" ]; then
	chsh -s $_s
fi
