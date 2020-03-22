xsetroot -solid '#ffffff'

if [ "$(hostname)" != echo ]; then
    setxkbmap -option "caps:swapescape"
fi

xrdb -merge ~/.Xresources

unclutter&

pkill -u $(id -u) redshift
redshift -l 59.91:10.74&

exec herbstluftwm --lock
