iOS iSH
=======

iSH is a bare-bones Alpine Linux userland using usermode x86 emulation
and syscall translation.

Install
-------

Install iSH from TestFlight.

Setup and config
----------------

1. Install prerequisites:

    ```sh
    apk add git openssh-client curl
    ```

2. Setup SSH keys:


    ```sh
    ssh-keygen -t ed25519
    cat .ssh/id_ed25519.pub | curl -F 'sprunge=<-' http://sprunge.us
    ```

3. Clone this repo:

    ```sh
        cd ~/src
        git clone git@github.com:uggedal/susc
        cd susc
    ```

4.  Fill a `env` file in the root of the chekcout of this repo
with the following environment variables:

    ```sh
    export HEADLESS=yes
    ```

5. Run system setup and configuration:

    ```sh
    ./ios.sh
    ```

9. Run user setup and config:

    ```sh
    ./user.sh
    ```
