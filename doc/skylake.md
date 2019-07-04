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

    apt install --no-install-recommends git openssh-server sudo
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
