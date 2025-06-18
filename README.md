# DART Cursor Notifier

An AutoHotkey v2 script that uses the Dublin Area Rapid Transit API to make users aware of when their train is coming by changing the cursor color.

## Purpose

This script lets folks in Dublin know when they need to leave their desk to catch their train without constantly checking the timetable. It provides a visual cue by changing the cursor color, so it's always in view while working. 

Green - ![image](https://github.com/user-attachments/assets/0d4e52aa-705a-4e59-afb6-5bd1fe9648c8)

Yellow - ![image](https://github.com/user-attachments/assets/a47af124-a86a-40e4-8962-9e6e27298c1c)

Red - ![image](https://github.com/user-attachments/assets/49026965-10dc-441a-bb4d-0547756dae46)

Every minute the API updates and presents a tooltip that gives information on the current train station and the next train in your direction 

![image](https://github.com/user-attachments/assets/9e8eada8-5808-4904-868a-39e0dd0a0645)

The script runs in the background, but the mouse cursor only changes during the active winwdow for checking Dart times so you're not always looking at a technicolour mouse. 

## Features

- **Station Selection**: Select any DART station through the settings GUI, organized by sections (Northern, City Centre, Southern)

- **Direction Filtering**: Choose to filter for northbound or southbound trains - reccomended. 

- **Customizable Activation Hours**: Set specific start and end times for when the script should be active - 17:10 - 18:00 for example. 

- **API Integration**: Makes minute-by-minute API calls to the Irish Rail API to retrieve real-time train arrival data 

## API Website & Documentation
https://api.irishrail.ie/realtime/

- **Color-Coded System Cursors**: The cursor changes color to indicate train arrival status:

  - **Green**: Plenty of time to catch your train
  - **Yellow**: Getting close - you should leave soon
  - **Red**: You won't make it to this train, consider the next one

- **Comprehensive Settings UI**: Easily configure all aspects through a tabbed settings interface

- **Auto-Resume**: The script automatically deactivates outside configured hours and resumes at the next start time

## Technical Implementation

This script changes all system cursor images based on the train arrival time. It reverts to the original mouse when the script stops or when the active window ends. 

## Usage

1. Run the script
2. If it's your first time, you'll be prompted to configure your settings:
   - Select your DART station e.g. station close to the office
   - Choose a direction filter (optional; reccomended)
   - Set your preferred time thresholds
   - Configure active hours for cursor monitoring
3. The script will activate during your configured hours and begin checking train times
4. Your cursor will change color based on train arrival times:
   - Green when you have plenty of time
   - Yellow when you need to start thinking about leaving
   - Red when it's too late for the current train (will automatically progress to next train)
5. Access the simple tray menu for quick actions:
   - Check train times now
   - Open the comprehensive settings
   - Restore default cursors

## Customization
You can customize:
- Your station and direction preference

# Station Selection
![image](https://github.com/user-attachments/assets/a3cfca46-e041-4126-822b-e57bbb070b84)

- Time thresholds for cursor color changes:
  - Minimum catchable time (when cursor turns red)
  - Yellow warning threshold
  - Red display duration before moving to next train
# Timing Settings
![image](https://github.com/user-attachments/assets/4072a7a4-7d95-4e64-b578-b3306b4aab5e)

- Active hours during which the script will monitor train times
# Schedule Settings
![image](https://github.com/user-attachments/assets/b49da87b-8e66-42ca-a8b1-fb638bc1d5d9)

## Credits

Takes inspiration from https://github.com/ivan-the-terrible/bloodsugar-cursor

