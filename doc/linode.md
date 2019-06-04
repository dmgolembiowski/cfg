Linode
======

Install
-------

Create a new Debian Linode node.

Setup and config
----------------

1. Configure the pre-installed system:

    ```sh
    cat <<EOF >/etc/apt/apt.conf.d/norecommends
    APT::Install-Recommends "false";
    APT::AutoRemove::RecommendsImportant "false";
    APT::AutoRemove::SuggestsImportant "false";
    EOF
    apt autoremove
    apt purge --autoremove pciutils laptop-detect discover
    apt purge --autoremove installation-report popularity-contest
    apt purge --autoremove lsof telnet hdparm debian-faq
    apt purge --autoremove apt-listchanges doc-debian traceroute
    apt purge --autoremove netcat-traditional krb5-locales
    apt purge --autoremove iotop mtr-tiny nano sysstat whois
    apt purge --autoremove debconf-i18n keyboard-configuration
    apt purge --autoremove python3-reportbug
    apt purge --autoremove python python-minimal python2.7-minimal
    apt purge $(dpkg -l | grep '^rc' | awk '{print $2}')

    cat <<EOF >/etc/apt/sources.list
    deb http://ftp2.de.debian.org/debian buster main contrib non-free
    deb http://ftp2.de.debian.org/debian buster-updates main contrib non-free
    deb http://security.debian.org buster/updates main
    EOF

    apt update && apt dist-upgrade

    echo $HOSTNAME > /etc/hostname
    hostname -F /etc/hostname
    echo 127.0.0.1 $FQDN $HOSTNAME >> /etc/hosts

    apt install git
    ```

2. Create a unprivileged user:

    ```sh
    useradd -s /bin/bash -mG sudo $u

    chmod 2750 /home/$u
    mkdir /home/$u/.ssh
    cp someplace/authorized_keys /home/$u/.ssh/
    chown -R $u: /home/$u/.ssh/
    ```

3. Remove root ssh login:

    ```sh
    sed -i 's/^\(PermitRootLogin\) yes/#\1 prohibit-password/' \
        /etc/ssh/sshd_config
    systemctl restart ssh
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
      - tls
      - mailsrv
      - www
    nftables:
      tcp_accept:
       - http
       - https
       - smtp
       - 6697  # znc
      tcp_limit:
        - ssh
    irc:
      nick: you
      oftc:
        join: '#chan1,#chan2'
      freenode:
        sasl: qwer1234
        join: '#chan1,#chan2'
    tls:
      - subj: domain.tld
        alt: '*.domain.tld'
    mail:
      domain: youdomain.tld
      owner: youruser
    linode:
      token: abcdefgh
    ```

6. Run system setup and configuration:

    ```sh
    ./system.sh
    ```

7. Run user setup and config:

    ```sh
    ./user.sh
    ```

Manual steps
------------

### Letsencrypt

1. Create a letsencrypt account:

    ```sh
    uacme -y new your@email.tld
    ```

2. Issue all confugured certs:

    ```sh
    /etc/periodic/daily/uacme
    ```
