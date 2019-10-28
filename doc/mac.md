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
- Internet Accounts
  - Exchange
    - Use with: mail, calendars, contacts
- Security and privacy
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
   - Modifier keys (for every keyboard)
     - Caps lock -> Escape
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
- Date and Time
  - Clock
    - Show the day of week: disable
- Sharing
  - Computer's Name: hostname

### Mission Control

- Add 4 spaces

### Amethyst

- Launch Amethyst (will fail)
- System Preferences -> Security & Privacy:
  - General:
    - Open Amethyst anayway
  - Privacy -> Accessibility:
    - Add and whitelist Amethyst
- Amethyst menubar:
  - Start Amethyst at startup

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
- Advanced
  - Smart search field: show full website address
  - Show develop menu in menu bar

### Menubar

- Battery: Show percentage

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

### Webex Teams

- Preferences
  - Start Webex Teams when my computer starts: disable

### Latest

- View
  - Show installed updates
  - Show version description

### Amphetamine

- Preferences
  - Launch Amphetamine at login
  - Hide Amphetamine in Dock