# DART Cursor Notifier

<div align="left">
<img src="https://github.com/user-attachments/assets/70ecc62a-2056-403d-8a0a-ebeb2d82e827" alt="Lyle Lanley" width="150" align="right" style="margin-left: 20px; margin-bottom: 20px;"/>

Are you tired of looking up the DART times on your phone? Do you get stuck working on a project at work only to realise you wont make your train before it leaves the station?

Never miss your train again! The `DART_Cursor.ahk` script uses the genuine, bonafide, electrified, Irish Rail API to put your train times directly on your screen! 

`DART_Cursor.ahk` is an AutoHotkey v2 script that uses the Irish Rail API to provide visual train arrival notifications by changing your cursor color. 

*Inspired by [ivan-the-terrible's bloodsugar-cursor](https://github.com/ivan-the-terrible/bloodsugar-cursor) using the same methods and cursor library.*

</div>

## How It Works

This script helps Dublin commuters know when to leave their desk to catch their train without constantly checking timetables. It provides an always-visible cue by changing the cursor color based on train arrival times.

| Status | Cursor | Meaning |
|--------|---------|---------|
| **Green** | ![image](https://github.com/user-attachments/assets/0d4e52aa-705a-4e59-afb6-5bd1fe9648c8) | Plenty of time to catch your train |
| **Yellow** | ![image](https://github.com/user-attachments/assets/a47af124-a86a-40e4-8962-9e6e27298c1c) | Getting close - you should leave soon |
| **Red** | ![image](https://github.com/user-attachments/assets/49026965-10dc-441a-bb4d-0547756dae46) | Too late for this train, showing next one |

The script updates every minute via the [Irish Rail API](https://api.irishrail.ie/realtime/) and shows a tooltip with current station info and next train details:

![Dart_Cursor_API_Update](https://github.com/user-attachments/assets/f3ae050a-828a-4461-85b3-474f06cf4064)

*Note: The cursor only changes during your configured active hours, so you're not always looking at a colored cursor.*

## Key Features

- **Smart Station Selection**: Choose any DART station in Dublin through an organized GUI (Northern, City Centre, Southern sections)
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
3. **Let it work** - The script monitors during configured hours and changes cursor colors automatically
4. **Use the tray menu** for quick access to:
   - Instant train time check
   - Settings adjustment
   - Cursor restoration

## Customization Options
### Station & Direction
Configure your preferred station and travel direction:

![image](https://github.com/user-attachments/assets/a3cfca46-e041-4126-822b-e57bbb070b84)

### Timing Thresholds
Set when cursor colors change based on train arrival times:

![image](https://github.com/user-attachments/assets/4072a7a4-7d95-4e64-b578-b3306b4aab5e)

### Active Schedule
Define when the script monitors train times:

![image](https://github.com/user-attachments/assets/b49da87b-8e66-42ca-a8b1-fb638bc1d5d9)

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
