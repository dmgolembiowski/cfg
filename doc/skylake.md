Skylake Server
==============

Install
-------

1. Download non-free Buster netinst iso with firmware for Intel wireless.
2. Write the image to a USB stick:

    ```sh
    dd bs=4M if=firmware-buster*.iso of=/dev/sda status=progress oflag=sync
    ```
3. Boot the installer and complete the following steps:
    - Advanced options
    - Expert install
    - Location: other -> Europe -> Norway
    - Configure the network:
        - Hostname: yourhostname
        - Domain name: yourdomain
    - Setup users and passwords:
      - Fill in full name, username and password
    - Partition disks:
        - Manual
        - Create 512M ESP on sda and sdb
        - Create RAID partition on the rest of sda and sdb
        - Assemble sda2/sdb2 RAID
        - Use assembed RAID as ext4 FS mounted on /
    - Install the base system:
        - targeted: only include drivers needed for this system
    - Configure the package manager:
        - Disable deb-src
        - Enable repos for:
            - security updates
            - release updates
            - backported software
    - Configuring discover:
        - Install security updates automatically
    - Software selection:
        - Disable all tasks

4. Reboot into the newly created system.

Setup and config
----------------

1. Setup essential packages:

    ```sh

    apt install --no-install-recommends git openssh-server sudo cryptsetup-run
    ```
2. Setup disks:

    ```sh
    for disk in sdc sdd sde sdf; do
        sgdisk -Z /dev/$disk
    done

    gdisk /dev/sdc <<EOF
    o
    Y
    n


    fd00
    w
    Y
    EOF

    sgdisk --backup=table /dev/sdc
    for disk in sdd sde sdf; do
        sgdisk --load-backup=table /dev/$disk
    done

    mdadm --create --verbose /dev/md2 --level=5 --raid-devices=4 /dev/sdc1 /dev/sdd1 /dev/sde1 /dev/sdf1

    cryptsetup luksFormat /dev/md2
    dd bs=512 count=4 if=/dev/urandom of=/etc/cryptkey_md2 iflag=fullblock
    cryptsetup luksAddKey /dev/md2 /etc/cryptkey_md2
    cryptsetup luksOpen --key-file /etc/cryptkey_md2 /dev/md2 md2_crypt

    mkfs.ext4 -m1 -E lazy_itable_init=0 -L data /dev/mapper/md2_crypt

    cat <<EOF >>/etc/crypttab
    md2_crypt /dev/md2 /etc/cryptkey_md2 luks
    EOF

    mkdir /data
    cat <<EOF >>/etc/fstab
    md2_crypt /dev/md2 /etc/cryptkey_md2 luks
    EOF
    ```


3. Setup unprivileged user:

    ```sh
    mkdir /home/$u/.ssh
    cp someplace/authorized_keys /home/$u/.ssh/
    chown -R $u: /home/$u/.ssh/
    usermod -aG sudo $u
    ```

4. Clone this repo:

    ```sh
    cd ~/src
    git clone git@github.com:uggedal/cfg
    cd cfg
    ```

5. Fill a `hostname.yml` file under `env/`
with the following environment variables:

    ```sh
    roles:
      - server
      - media
    ```

6. Run system setup and configuration:

    ```sh
    ./system.sh
    ```

7. Run user setup and config:

    ```sh
    ./user.sh
    ```
