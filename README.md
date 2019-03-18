System and User Setup and Config
================================

Bootstrap
---------

Create a user and clone this repo with:

    useradd -m -G wheel,adm $USER
    passwd $USER
    mkdir ~/src ~/.ssh
    cp $SAFEPLACE/id_* ~/.ssh
    su - $USER
    cd ~/src
    git clone git@github.com:uggedal/susc
    cd susc

Setup and config
----------------

Fill a `env` file with the following environment variables:

    export AUTOLOGIN_USER=your_username

Run system setup and config:

    su -c ./system.sh

Run user setup and config:

    ./user.sh

Vim
---

Install plugins and plugin dependencies with:

    :PlugInstall
    :GoInstallBinaries
