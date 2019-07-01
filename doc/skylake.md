Skylake Server
==============

Install
-------

1. Boot rescure CD with debootstrap and complete the following steps:

    ```sh
    # TODO: copy over disk setup notes
    # TODO: doc fstab
    # TODO: doc crypttab

    cryptsetup luksFormat /dev/md1
    cryptsetup luksOpen /dev/md1 md1_crypt
    mkfs.ext4 -m1 -E lazy_itable_init=0 -L root /dev/mapper/md1_crypt
    mount /dev/mapper/md1_crypt /mnt

    mkfs.ext4 -m1 -E lazy_itable_init=0 -L boot /dev/md0
    mkdir /mnt/boot
    mount /dev/md0 /mnt/boot

    mkfs.vfat -F 32 /dev/sda1
    mkdir /mnt/boot/efi
    mount /dev/sda1 /boot/efi

    debootstrap --arch=amd64 --components=main,contrib,non-free --include=openssh-server,cryptsetup,mdadm buster /mnt http://deb.debian.org/debian

    mount --rbind /dev /mnt/dev
    mount --rbind /sys /mnt/sys

    chroot /mnt /bin/bash <<EOCHROOT
    mount none /proc -t proc

    passwd

    systemctl enable ssh

	cat <<EOF >/etc/systemd/network/wired.network
    [Match]
    Name=enp1s0

    [Network]
    DHCP=ipv4
    EOF

    systemctl disable networking
    systemctl enable systemd-networkd

    systemctl enable systemd-resolved
    ln -sf /run/systemd/resolve/resolv.conf /etc/resolv.conf

    mdadm --detail --scan > /etc/mdadm/mdadm.conf
    echo MAILADDR root >> /etc/mdadm/mdadm.conf

    apt-get -y --no-install-recommends install dropbear-initramfs linux-image-amd64 grub-efi-amd64 firmware-realtek

    echo "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBEtPmZXsi8JvjI1oWmKY6HIezRcQLa2WThwrnfCQrr2xnXnv3NJWR2TKiWBHCNF2HlvWU/d6kr0ZPrcCG2mGq2A= eu@krypton" >/etc/dropbear-initramfs/authorized_keys
    update-initramfs -u

    grub-install --target=x86_64-efi --efi-directory=/boot/efi --bootloader-id=debian --recheck --debug
    update-grub

    EOCHROOT
    ```

2. Reboot into the newly created system.

Setup and config
----------------

1. Setup essential packages:

    ```sh

    apt install --no-install-recommends git
    ```

2. Setup unprivileged user:

    ```sh
    mkdir /home/$u/.ssh
    cp someplace/authorized_keys /home/$u/.ssh/
    chown -R $u: /home/$u/.ssh/
    usermod -aG sudo $u
    ```

3. Clone this repo:

    ```sh
    cd ~/src
    git clone git@github.com:uggedal/cfg
    cd cfg
    ```

4. Fill a `hostname.yml` file under `env/`
with the following environment variables:

    ```sh
    roles:
      - server
      - media
    ```

5. Run system setup and configuration:

    ```sh
    ./system.sh
    ```

6. Run user setup and config:

    ```sh
    ./user.sh
    ```
