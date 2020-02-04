xsetroot -solid '#4d4d4c'

if [ "$(hostname)" != echo ]; then
	setxkbmap -option "caps:swapescape"
fi

xrdb -merge ~/.Xresources

unclutter&

exec emacs
