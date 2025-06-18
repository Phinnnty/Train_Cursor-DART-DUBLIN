# DART Cursor Notifier

<div align="left">
<img src="https://github.com/user-attachments/assets/70ecc62a-2056-403d-8a0a-ebeb2d82e827" alt="Lyle Lanley" width="170" align="right" style="margin-left: 40px; margin-bottom: 40px;"/>

Are you tired of looking up the DART times on your phone? Do you get stuck working on a project at work only to realise you wont make your train before it leaves the station?

Never miss your train again! The `DART_Cursor.ahk` script uses the genuine, bonafide, electri-fied, Irish Rail API to put your train times directly on your screen! 

The `DART_Cursor.ahk` is an AutoHotkey v2 script that uses the Irish Rail API to provide visual train arrival notifications.

</div>

*Inspired by [ivan-the-terrible's bloodsugar-cursor](https://github.com/ivan-the-terrible/bloodsugar-cursor) using the same methods and cursor library.*

## How's It Work?

This script helps Dublin commuters know when to leave their desk to catch their train without constantly checking timetables. Users select the timeframe they want the script to work and during that period it gives periodic updates via the [Irish Rail API](https://api.irishrail.ie/realtime/) it provides a visual cue to indicate train arrival times at your train station by changing the colour of your cursor and generating a short lived tooltip with the next train's details. 

![Dart_Cursor_API_Update](https://github.com/user-attachments/assets/f3ae050a-828a-4461-85b3-474f06cf4064)

| Status | Cursor | Meaning |
|--------|---------|---------|
| ***Green*** | ![image](https://github.com/user-attachments/assets/0d4e52aa-705a-4e59-afb6-5bd1fe9648c8) | **Relax!** Plenty of time to catch your train - no rush needed |
| ***Yellow*** | ![image](https://github.com/user-attachments/assets/a47af124-a86a-40e4-8962-9e6e27298c1c) | **Time to go!** Your train arrives in 7-10 minutes - if it takes you that long to reach the station, leave now! |
| ***Red*** | ![image](https://github.com/user-attachments/assets/49026965-10dc-441a-bb4d-0547756dae46) | **Too late!** This train arrives in less than your travel time to the station - we'll show the next available train instead |

> ***Note***: All timing thresholds are fully customizable in the settings - adjust them to match your actual travel time to the station!

## Key Features

- **Station Selection**: Choose any DART station in Dublin through an organized GUI (Northern, City Centre, Southern sections)
- **Direction Filtering**: Filter for northbound/southbound trains (recommended as otherwise you'll see every train in either direction)
- **Flexible Scheduling**: Set specific active hours (e.g., 17:10-18:00) for automatic operation
- **Real-time Updates**: Minute-by-minute API calls for current train data
- **Auto-Resume**: Automatically activates/deactivates based on your schedule
- **Comprehensive Settings**: Easy configuration through tabbed interface
- **System Integration**: Changes all system cursors, reverts when stopped

## Quick Start

1. **Run the script** - First-time setup will guide you through configuration
2. **Configure your preferences:**
   - Select your DART station (e.g., one near your office)
   - Choose direction filter (recommended)
   - Set time thresholds for color changes
   - Configure active monitoring hours
   - **Optional**: Enable Work/Home mode and configure both stations
3. **Select monitoring mode:**
   - **Single Station**: The script monitors your selected station during configured hours
   - **Work/Home Mode**: Use radio buttons to select Home or Work mode, then click OK to apply
4. **Let it work** - The script monitors during configured hours and changes cursor colors automatically
5. **Use the tray menu** for quick access to:
   - Instant train time check
   - Work/Home mode switching (if enabled)
   - Settings adjustment
   - Cursor restoration

## Customization Options
### Station & Direction
Configure your preferred station and travel direction:
- Pick a station a direction! 

#### Work/Home Mode
The script includes a powerful Work/Home mode feature for commuters who travel between two regular stations:

**Setup Process:**
1. **Enable the Feature**: Check "Enable Work/Home Mode (quick station switching)" to activate the functionality
2. **Configure Home Station**: Set your home station and direction (typically for evening commute)
3. **Configure Work Station**: Set your work station and direction (typically for morning commute)
4. **Select Active Mode**: Use the radio buttons to choose which mode is currently active:
   - **Home Mode**: Monitors your home station 
   - **Work Mode**: Monitors your work station

**How to Use:**
- **Settings Interface**: Use the radio buttons in the Station tab to select Home/Work mode
- **Tray Menu**: Right-click the system tray icon for instant mode switching
- **Real-time Display**: Current station and direction are shown based on your selection

**Benefits:**
- No need to manually change stations when switching between morning/evening commutes
- Quick toggling between two preset station configurations
- Remembers your preferred directions for each station
- Visual confirmation of current active mode and station

![image](https://github.com/user-attachments/assets/74c64ff2-a53d-46e7-b90d-282b9567a2f4)

### Timing Thresholds
Set when cursor colors change based on train arrival times:
- This is entirely customisable by the user.
- Set the thresholds to suit you!
   - ****Minimum Catchable Time**** indicates the latest you think you can leave (if a train arrives in less time that this, the cursor turns red)
   - ****Yellow Warning Threshold**** indicates if you leave now you'll arrive just before your train does.
   - ****Red Display Time**** indicates how long to show the red cursor before moving on to the next train headed your direction!
   - ****API Check Freqeuncy**** indicates how often we call the Irish Rail API and update the cursor. 

![image](https://github.com/user-attachments/assets/2dbf52d2-0580-481b-8bce-c9af2eca01f6)

### Active Schedule
Define when the script monitors train times:
> ***Note***: The cursor only changes during your configured active hours, so you're not always looking at a colored cursor.*

![image](https://github.com/user-attachments/assets/92ea8886-25b0-423c-9975-b5595b751659)

## Installation

### AutoHotkey Setup
1. Visit [autohotkey.com](https://www.autohotkey.com)
2. Download the latest release
3. Extract and run the installer
4. Double-click any `.ahk` script to run it

### Script Usage
Simply download and run `DART_Cursor.ahk` - the setup wizard will guide you through initial configuration.

#### Run on startup 
1. If you want the scripts to run at startup you can add them to the startup directory on your machine.
2. Create a shortucut to your script by right clicking, select create shortcut from the menu.
4. Save this shortcut in the startup folder on your computer so that you dont need to manually boot it after each restart.
5. Startup folder is found by searching "Run" on the task bar and inputting the following cmd : shell:startup
6. Move the shortcut you created to this location. It will now run on startup. 

## Technical Notes
- Changes all system cursor images based on train timing
- Reverts to original cursors when stopped - yuo dont want to be stuck with a green cursor all day. 
- Easily extensible to other Irish Rail stations (TODO)

---
*Inspired by [ivan-the-terrible's bloodsugar-cursor](https://github.com/ivan-the-terrible/bloodsugar-cursor)*
