Linode
======

Install
-------

Create a new Alpine Linux Linode node.

Setup and config
----------------

1. Configure the pre-installed system:

    ```sh
    echo $HOSTNAME > /etc/hostname
    hostname -F /etc/hostname
    echo 127.0.0.1 $FQDN $HOSTNAME >> /etc/hosts

    apk del acct iotop mtr nano syslinux sysstat

    apk update && apk upgrade

    apk add bash bash-completion git
    ```

2. Create a unprivileged user:

    ```sh
    addgroup $u
    adduser -s/bin/bash -G $u $u
    adduser $u wheel
    adduser $u abuild
    chmod 2750 /home/$u
    mkdir /home/$u/.ssh
    cp someplace/authorized_keys /home/$u/.ssh/
    chown -R $u: /home/$u/.ssh/
    ```

3. Remove root ssh login:

    ```sh
    sed -i 's/^\(PermitRootLogin\) yes/#\1 prohibit-password/' \
        /etc/ssh/sshd_config
    /etc/init.d/sshd restart
    ```

4. Login as the unprivileged user and clone this repo:

    ```sh
    mkdir ~/src
    cd ~/src
    git clone git@git.sr.ht:~eju/cfg
    cd cfg
    ```

5. Fill a `env` file in the root of the chekcout of this repo
with the following environment variables:

    ```sh
    roles:
      - server
      - vm
      - dev
      - mail
      - build
      - irc
      - mailsrv
    nftables:
      tcp_accept:
       - smtp
       - 6697  # znc
      tcp_limit:
        - ssh
    irc:
      nick: you
      freenode:
        sasl: qwer1234
        join: '#chan1,#chan2'
    mail:
      domain: youdomain.tld
      owner: youruser
    ```

6. Run system setup and configuration:

    ```sh
    ./system.sh
    ```

7. Run user setup and config:

    ```sh
    ./user.sh
    ```
