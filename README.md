System and User Setup and Config
================================

Provisioning of an Arch Linux based development setup on
a ThinkPad X1 Carbon 6th gen (Coffe Lake).

BIOS
----

Update BIOS to >= 0.1.37.

- Config
  - Keyboard/Mouse
    - Fn and Ctrl Key swap: Enabled
  - Power
    - Sleep State: Linux
- Security
  - I/O Port Access
    - Wireless WAN: Disabled
    - Memory Card Slot: Disabled
    - Fingerprint Reader: Disabled
  - Secure Boot Configuration
    - Secure Boot: Disabled

Install
-------


Boot Arch Linux Installer and run the following commands:

    sgdisk -Z /dev/nvme0n1

    gdisk /dev/nvme0n1 <<EOF
    n


    512M
    ef00
    n
    
    
    
    
    w
    YES
    EOF

    cryptsetup --type luks2 luksFormat /dev/nvme0n1p2
    cryptsetup luksOpen /dev/nvme0n1p2 cryptroot

    mkfs.vfat -F32 /dev/nvme0n1p1
    mkfs.ext4 -m1 -L root /dev/mapper/cryptroot

    mount /dev/mapper/cryptroot /mnt
    mkdir /mnt/boot
    mount /dev/nvme0n1p1 /mnt/boot

    pacstrap /mnt base linux-lts terminus-font intel-ucode git wpa_supplicant

    genfstab -U /mnt >> /mnt/etc/fstab

    arch-chroot /mnt

    ln -sf /usr/share/zoneinfo/Europe/Oslo /etc/localtime
    hwclock --systohc
    sed -i 's/^#\(en_US.UTF\)/\1/' /etc/locale.gen
    locale-gen
    echo LANG=en_US.UTF-8 > /etc/locale.conf
    echo FONT=ter-118n > /etc/vconsole.conf
    echo $HOSTNAME > /etc/hostname
    echo 127.0.0.1 $FQDN $HOSTNAME >> /etc/hosts

    hs='systemd autodetect keyboard sd-vconsole modconf block sd-encrypt filesystems fsck'
    sed -i "s/^\(HOOKS=\).*/\1($hs)/" /etc/mkinitcpio.conf
    mkinitcpio -p linux-lts

    bootctl install

    uuid=$(blkid -s UUID /dev/nvme0n1p2 | awk '{ print $2 }' | tr -d '"')

    cat <<EOF > /boot/loader/entries/arch-lts.conf
    title  Arch Linux LTS
    linux  /vmlinuz-linux-lts
    initrd  /intel-ucode.img
    initrd /initramfs-linux-lts.img
    options rd.luks.name=$uuid=cryptroot rd.luks.options=discard root=/dev/mapper/cryptroot rw quiet
    EOF

    passwd

Bootstrap
---------

Setup wireless:

    cat <<EOF >/etc/wpa_supplicant/wpa_supplicant-wlp2s0.conf
    network={
      ssid="foo"
      psk="bar"
    }
    EOF
    systemctl enable wpa_supplicant@wlp2s0
    systemctl start wpa_supplicant@wlp2s0

    cat <<EOF >/etc/systemd/network/wireless.network
    [Match]
    Name=wlp2s0

    [Network]
    DHCP=ipv4
    EOF

    systemctl enable systemd-networkd systemd-resolved
    systemctl start systemd-networkd systemd-resolved

    ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf

Create a user:

    useradd -m -G wheel,adm,video,audio $USER
    passwd $USER

Clone this repo with with the newly created user:

    mkdir ~/src ~/.ssh
    cp $SAFEPLACE/id_* ~/.ssh
    cd ~/src
    git clone git@github.com:uggedal/susc
    cd susc

Setup and config
----------------

Fill a `env` file in the root of the chekcout of this repo
with the following environment variables:

    export AUTOLOGIN_USER=your_username
    export SSH_TUNNEL_HOST=your.host.name
    export TODOIST_API_TOKEN=abcdef1234567890abcdef1234567890abcdef12
    export TASKD_CREDENTIALS=inthe_am/username/aaaaaaaa-bbbb-cccc-dddd-eeeeeeeeeeee

Run system setup and config with root:

    su -c ./system.sh

Build and install AUR packages:

    ./aur.sh

Run user setup and config with your normal user:

    ./user.sh

Manual configuration
--------------------

Hopefully this will be a very short section.

### Vim

Install plugins and plugin dependencies with:

    :PlugInstall
    :GoInstallBinaries

### Taskwarrior

Download the following from inthe.am:

- ~/.config/task/private.certificate.pem
- ~/.config/task/private.key.pem
- ~/.config/task/ca.cert.pem

### Firefox

First login to Firefox Sync so that the last part of this
section is handled automatically. Then configure the following:

- Customize
  - Remove flexible spaces
  - Density: compact
  - Remove: home and refresh buttons
  - Move the following to overflow: library, sidebars, all extensions
    except 1Password
- Alternative profile
  - firefox -ProfileManager
  - Create priv profile
  - Configure with same settings as above except:
    - Preferences
      - Manual proxy configureation
        - SOCKS Host: 127.0.0.1:1337

Without Firefox Sync the following tasks needs to be completed:

- Install and configure extensions:
  - Tree Style Tab
  - uBlock Origin
    - https://github.com/yourduskquibbles/webannoyances
  - Tridactyl
  - 1Password
  - Nano Defender
- Preferences:
  - General
    - Startup
      - Select: Restore previous session
    - Tabs
      - Deselect: Ctrl+Tab cycles through tabs in recently used order
    - Fonts & Colors
      - Default font: Noto Serif
      - Advanced
        - Serif: Noto Serif
	- Sans-serif: Noto Sans
	- Monospace: Noto Sans Mono
  - Privacy & Security
    - Disable: Ask to save logins and passwords
  - Firefox Account
    - Only sync:
      - Add-ons
      - Preferences
