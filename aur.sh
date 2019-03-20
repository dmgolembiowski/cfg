#!/bin/sh -e

ROOT=$(cd "$(dirname "$0")"; pwd -P)
CHROOT=$ROOT/chroot/root

AUR_URL='https://aur.archlinux.org'

AUR_PACKAGES='
	brightnessctl
	plex-media-player
'

mkdir -p $CHROOT

[ -d $CHROOT/root ] || mkarchroot $CHROOT/root base-devel

arch-nspawn $CHROOT/root pacman -Syu

for p in $AUR_PACKAGES; do
	(
		cd $ROOT/aur
		curl $AUR_URL/cgit/aur.git/snapshot/$p.tar.gz |
			tar xz
	)
done

for d in $ROOT/aur/*; do
	n=$(basename $d)
	v=$(awk -F= '/^pkgver=/ { print $2 }' $d/PKGBUILD)
	r=$(awk -F= '/^pkgrel=/ { print $2 }' $d/PKGBUILD)
	p=$d/${n}-${v}-${r}-x86_64.pkg.tar.xz

	if [ -e $p ]; then
		continue
	fi

	(
		cd $d
		makechrootpkg -c -r $CHROOT
	)

	sudo pacman -U $p
done
