System and User Setup and Config
================================

Provisioning of an Arch Linux based development setup.

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

Manual configuration
--------------------

Hopefully this will be a very short section.

### Vim

Install plugins and plugin dependencies with:

    :PlugInstall
    :GoInstallBinaries

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
