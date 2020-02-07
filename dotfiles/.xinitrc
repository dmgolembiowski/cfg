xsetroot -solid '#4d4d4c'

if [ "$(hostname)" != echo ]; then
	setxkbmap -option "caps:swapescape"
fi

xrdb -merge ~/.Xresources

unclutter&

redshift -l 59.91:10.74&

exec emacs
