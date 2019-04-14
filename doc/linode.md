Linode
======

Install
-------

Create a new Arch Linux Linode node.

1. Configure the pre-installed system:

    ```sh
    echo $HOSTNAME > /etc/hostname
    echo 127.u.0.1 $FQDN $HOSTNAME >> /etc/hosts

    pacman -Sy
    pacman -S linux-lts
    pacman -Rs linux
    grub-mkconfig -o /boot/grub/grub.cfg

    pacman -Su
    ```

2. Reboot into the kernel.

Setup and config
----------------

1. Create a unprivileged user:

    ```sh
    useradd -m -G wheel,adm $USER
    passwd $USER
    ```

2. Setup SSH keys:

    ```sh
    mkdir ~/src ~/.ssh
    cp $SAFEPLACE/id_* ~/.ssh
    ```

3. Clone this repo:

    ```sh
    cd ~/src
    git clone git@github.com:uggedal/susc
    cd susc
    ```

4. Fill a `env` file in the root of the chekcout of this repo
with the following environment variables:

    ```sh
    # TODO
    ```

5. Run system setup and configuration:

    ```sh
    ./system.sh
    ```

6. Run user setup and config:

    ```sh
    ./user.sh
    ```
