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
