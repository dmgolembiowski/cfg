#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)
CHROOT=$ROOT/chroot/root

AUR_URL='https://aur.archlinux.org'

AUR_PACKAGES='
	needrestart
'

UPDATE=$([ "$1" != -u ] || echo yes)

mkdir -p $CHROOT

[ -d $CHROOT/root ] || mkarchroot $CHROOT/root base-devel

mkdir -p ~/.gnupg
echo 'keyserver-options auto-key-retrieve' > ~/.gnupg/gpg.conf

pkgf() {
	local d=$1
	local v=$(awk -F= '/^pkgver=/ { print $2 }' $d/PKGBUILD)
	local r=$(awk -F= '/^pkgrel=/ { print $2 }' $d/PKGBUILD)
	local e=$(awk -F= '/^epoch=/ { print $2 }' $d/PKGBUILD)

	if [ "$e" ]; then
		e=$e:
	fi

	echo $d/$(basename $d)-${e}${v}-${r}-*.pkg.tar.xz
}

if [ "$UPDATE" ]; then
	for p in $AUR_PACKAGES; do
		(
			cd $ROOT/aur
			curl $AUR_URL/cgit/aur.git/snapshot/$p.tar.gz |
				tar xz
		)
	done
else
	arch-nspawn $CHROOT/root pacman -Syu

	for n in $AUR_PACKAGES; do
		d=$ROOT/aur/$n
		p=$(pkgf $d)

		if [ -e $p ]; then
			continue
		fi

		(
			cd $d
			case $n in
				*)
					makechrootpkg -c -r $CHROOT
					;;
			esac
		)

		sudo pacman -U $p
	done
fi
