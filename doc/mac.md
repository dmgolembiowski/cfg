MacBook Pro
===========

Install
-------

- English language
- Norwegian location
- US English keyboard
- Disable iCloud Documents folders
- Dark Mode

Setup and config
----------------

1. Setup SSH keys:

    ```sh
    mkdir ~/src ~/.ssh
    cp $SAFEPLACE/id_* ~/.ssh
    ```

2. Clone this repo:

    ```sh
    cd ~/src
    git clone git@github.com:uggedal/cfg
    cd cfg
    ```

3. Fill a `hostname.yml` file under `env/`
with the following environment variables:

    ```sh
    roles:
      - dev
      - work
    ```

3. Run system setup and configuration:

    ```sh
    ./mac.sh
    ```

7. Run user setup and config:

    ```sh
    ./user.sh
    ```

Manual configuration
--------------------

### Firmware

- Setup firmware password: https://support.apple.com/en-gb/HT204455

### System Preferences

- Desktop and screen saver
    - Screen saver
        - Start after: Never
- Dock
    - Decrease size slightly
    - Position left
    - Automatically hide
- Mission Control
    - Disable automatic rearranging of spaces based on usage
- Siri
    - Listen for "Yey Siri": disable
    - Show Siri in menu bar: disable
- Spotlight
    - Search results
        - Disable all except:
            - Applications
            - Calculator
            - Conversion
            - Definition
            - System preferences
    - Privacy
        - Ignore ~/<yourname>
- Internet Accounts
    - Exchange
        - Use with: mail, calendars, contacts
- Security and privacy
    - General
        - Require password immediately after sleep or screen saver begins
    - Firewall
        - Turn on firewall
        - Firewall options
            - Turn on stealth mode
- Network
    - Advanced
        - DNS
            - 1.1.1.1
            - 1.0.0.1
            - 2606:4700:4700::1111
            - 2606:4700:4700::1001
- Sound
    - Disable interface sound effects
- Keyboard
    - Keyboard
        - Modifier keys (for every keyboard)
        - Caps lock -> Escape
    - Shortcuts:
        - Launchpad & Dock: disable all
- Trackpad
    - Lookup & data detectors: disable
- Displays
    - For internal and external displays
        - Scaled: More Space
        - Night Shift
            - Subset to Sunrise
    - Show mirroring options in the menu bar when available: disable
- Energy saver
    - Power adapter
        - Turn display off after: 30min
        - Show battery status in menu bar: disable
- Date and Time
    - Clock
        - Show the day of week: disable
- Sharing
    - Computer's Name: hostname

### Mission Control

- Add 4 spaces

### Menubar

- Battery: Show percentage

### Latest

- View
    - Show installed updates
    - Show version description

### Hammerspoon

- Preferences
    - Launch at login
    - SHow dock icon: disable
- Privacy -> Accessibility:
    - Add and whitelist Hammerspoon

### Contexts

- Preferences
    - Apperance
        - Theme: Vibrant Dark
    - Sidebar:
        - Show sidebar on: No display
    - Command-Tab
        - Show windows from: Visible spaces

### Cursorer

- Launch Cursorer (will fail)
- System Preferences -> Security & Privacy:
    - General:
        - Open Cursorer anayway
- Preferences:
        - Hide idle cursor after 5 seconds

### Itsycal

- Preferences
    - General
        - Launch at login
        - First day of week: Monday
        - Add relevant calendars
        - Event list shows: 2 days
    - Apperance
        - Outline icon
        - Show calendar weeks
        - Use larger text

### Cpuinfo

- Launch Cpuinfo (will fail)
- System Preferences -> Security & Privacy:
    - General:
        - Open Cpuinfo anayway
- Menubar:
    - Start at login

### Monitor Contorl

- Preferences
    - Start MonitorControl at login
- Privacy -> Accessibility:
    - Add and whitelist MonitorControl

### Amphetamine

- Preferences
    - Launch Amphetamine at login
    - Hide Amphetamine in Dock

### ToothFairy

- Preferences:
    - Add AirPods Pro:
        - Change icon
    - Launch at login
    - Hide Dock icon

### Things 3

- Preferences:
    - General:
        - Group to-dos in the Today list by project or area: enable
    - Things Cloud:
        - Login

### Safari

- General
    - Safari opens with: all windows from last session
    - New window opens with: empty page
    - New tabs opens with: empty page
    - Remove history items: manually
- Tabs
    - Show website icons in tabs
- Autofill
    - Username and passwords: disable
- Extensions
    - Bitwarden
    - Vimari
    - Wipr (part 1, 2 and 3)
- Advanced
    - Smart search field: show full website address
    - Show develop menu in menu bar

### Wipr

- Download lists
- Enable automatic refresh

### Firefox

First login to Firefox Sync so that the last part of this
section is handled automatically. Then configure the following:

- Customize
    - Remove flexible spaces
    - Density: compact
    - Remove: home and refresh buttons
    - Move the following to overflow: library, sidebars, all extensions
      except Bitwarden, uBlock and TreeStyleTabs
- Preferences
    - Connection settings:
        - Manual proxy configureation
        - SOCKS Host: 127.0.0.1:1337
        - Proxy DNS when using SOCKS v5

Without Firefox Sync the following tasks needs to be completed:

- Install and configure extensions:
  - uBlock Origin
  - Tridactyl
  - Vimium-FF
  - Bitwarden
  - Bear
- Preferences:
    - General
        - Startup
            - Select: Restore previous session
        - Tabs
            - Deselect: Ctrl+Tab cycles through tabs in recently used order
        - Fonts & Colors
    - Privacy & Security
        - Disable: Ask to save logins and passwords
    - Firefox Account
        - Only sync:
            - Add-ons
            - Preferences

### Secure Pipes

- Preferences
    - General
        - Launch application at login
        - Disconnect/reconnect active connections on sleep/wake
    - Connections
        - New SOCKS Proxy:
            - Name
            - Address
            - User
            - Bind: localhost:1337
            - Options:
                - Use ssh identity file

### Reeder

- Setup
    - Self hosted Fever API
        - Host: <hostname>/fever/
        - Username
        - Password

### Spotify

- Settings
    - Music Quality
        - High quality streaming: enable
    - Display Options
        - Show unavailable songs in playlists: enable
        - Show friend activity: disable
    - Social
        - Disable all options
    - Local files
        - Disable all options
    - Atoplay
        - Autoplay similar songs when your music ends: disable
    - Startup and Window Behavior
        - Open Spotify automatically after you log into the computer: No

### Mail

- Exchange
    - Use with: mail, calendar, contacts
- Google
    - Use with: mail, calendar, contacts

### Calendar

- Preferences
    - General
        - Show Birthdays calendar: disable
        - Show Holidays calendar: disable
    - Advanced
        - Show events in year view
        - Show week numbers

### Mail

- Exchange
    - Use with: mail, calendar, contacts
- Google
    - Use with: mail, calendar, contacts

### Webex Teams

- Preferences
    - Start Webex Teams when my computer starts: disable
