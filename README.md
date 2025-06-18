# DART Cursor Notifier

An AutoHotkey v2 script that uses the Dublin Area Rapid Transit API to make users aware of when their train is coming by changing the cursor color.

## Purpose

This script lets folks at Murex Dublin Office know when they need to leave their desk to catch their train without constantly checking the timetable. It provides a visual cue by changing the cursor color, so it's always in view while working. 

The script runs in the background, but the mouse cursor only changes during the active winwdow for checking Dart times so you're not always looking at a technicolour mouse. 

## Features

- **Station Selection**: Select any DART station through the settings GUI, organized by sections (Northern, City Centre, Southern)

- **Direction Filtering**: Choose to filter for northbound or southbound trains - reccomended. 

- **Customizable Activation Hours**: Set specific start and end times for when the script should be active - 17:10 - 18:00 for example. 

- **API Integration**: Makes minute-by-minute API calls to the Irish Rail API to retrieve real-time train arrival data 

API Website & Documentation
>>>>> https://api.irishrail.ie/realtime/ <<<<<

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
- Time thresholds for cursor color changes:
  - Minimum catchable time (when cursor turns red)
  - Yellow warning threshold
  - Red display duration before moving to next train
- Active hours during which the script will monitor train times

## Credits

Takes inspiration from https://github.com/ivan-the-terrible/bloodsugar-cursor

