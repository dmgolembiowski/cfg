VMware dev VM
=============

Pre-install
-----------

* Drag ISO into VMware Fusion setup window.
* Select Linux -> Other Linux 5.x or later kernel 64-bit
* Select UEFI boot firmware.
* Save VM as <hostname>.
* Configure:
    * Processors and Memory:
        * 2048MB RAM
        * 4 processor cores.
    * Display: TODO
    * Hard disk:
        * 200GB size
    * Shared folder:
        * Share MacOS home dir

Install
-------

Boot VM into installer and complete the following steps.

1. Setup encrypted disk:

    ```sh
    sgdisk -Z /dev/sda

    gdisk /dev/sda <<EOF
    n


    256M
    ef00
    n




    w
    YES
    EOF

    cryptsetup --type luks2 luksFormat /dev/sda2
    cryptsetup luksOpen /dev/sda2 cryptroot

    mkfs.vfat -F32 /dev/sda1
    mkfs.ext4 -m1 -L root /dev/mapper/cryptroot
    ```

2. Bootstrap an Arch Linux system:

    ```sh
    mount /dev/mapper/cryptroot /mnt
    mkdir /mnt/boot
    mount /dev/sda1 /mnt/boot

    pacstrap /mnt base linux linux-firmware git openssh open-vm-tools inetutils

    genfstab -U /mnt >> /mnt/etc/fstab
    ```

3. Configure the bootstrapped system:

    ```sh
    arch-chroot /mnt

    ln -sf /usr/share/zoneinfo/Europe/Oslo /etc/localtime
    sed -i 's/^#\(en_US.UTF\)/\1/' /etc/locale.gen
    locale-gen
    echo LANG=en_US.UTF-8 > /etc/locale.conf
    echo $HOSTNAME > /etc/hostname
    echo 127.0.0.1 $FQDN $HOSTNAME >> /etc/hosts

    hs='systemd autodetect keyboard sd-vconsole modconf block sd-encrypt filesystems fsck'
    sed -i "s/^\(HOOKS=\).*/\1($hs)/" /etc/mkinitcpio.conf
    mkinitcpio -p linux

    bootctl install

    uuid=$(blkid -s UUID /dev/sda2 | awk '{ print $2 }' | tr -d '"')

    cat <<EOF > /boot/loader/entries/arch.conf
    title  Arch Linux
    linux  /vmlinuz-linux
    initrd /initramfs-linux.img
    options rd.luks.name=$uuid=cryptroot root=/dev/mapper/cryptroot rw quiet
    EOF

    # Set root password:
    passwd
    ```
4. Reboot into the newly created system.

Setup and config
----------------

1. Setup network and time (needed for DNSSEC validation):

    ```sh
    cat <<EOF >/etc/systemd/network/wired.network
    [Match]
    Name=ens33

    [Network]
    DHCP=ipv4
    EOF

    systemctl enable systemd-networkd systemd-resolved
    systemctl start systemd-networkd systemd-resolve

    ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf

    timedatectl set-ntp true
    ```

2. Create a unprivileged user:

    ```sh
    useradd -m -G wheel,adm,video,audio $USER
    passwd $USER
    ```

3. Setup SSH keys:

    ```sh
    mkdir ~/host
    vmhgfs-fuse -o auto_unmount .host:/<hostdir> host

    mkdir ~/src ~/.ssh
    cp host/.ssh/id_* ~/.ssh
    ```

4. Clone this repo:

    ```sh
    cd ~/src
    git clone git@github.com:uggedal/cfg
    cd cfg
    git clone git@github.com:uggedal/env
    git clone git@github.com:uggedal/aur
    ```

5. Fill a `hostname.yml` file under `env/`
with the following environment variables:

    ```sh
    roles:
      - vm
      - desktop
    autologin:
      user: eu
    ssh_tunnel:
      host: tunnel.host.tld
    ```

6. Run system setup and configuration:

    ```sh
    ./system.sh
    ```

7. Run user setup and config:

    ```sh
    ./user.sh
    ```
