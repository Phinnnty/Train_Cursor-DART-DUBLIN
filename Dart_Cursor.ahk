#Requires AutoHotkey v2.0
#SingleInstance Force

; ============================
; Dart Cursor - Train Arrival Notification via Cursor Color
; ============================
; Inspired by and uses resources from https://github.com/ivan-the-terrible/bloodsugar-cursor
; Made by Fintan M for  Dubliners (lol I'm moving to Canada so I wont be using it)
;
; Set the DART icon in the system tray
TraySetIcon(A_ScriptDir "\Dart.png")

; Global variables
global configFile := A_ScriptDir "\DartConfig.ini"
global logFile := A_ScriptDir "\DartCursor.log"
global preferredDirection := ""
global trainData := []
global currentStation := "Lansdowne Road"
global checkFrequency := 60000 ; Check every minute (60000ms)
global isActive := false
global currentTrainIndex := 1     ; Index of the train we're currently tracking
global minCatchableTime := 7      ; Minimum minutes needed to catch a train (red cursor)
global yellowThreshold := 10      ; Absolute minutes threshold for yellow warning
global redDisplayTime := 60000    ; How long to show red cursor before moving to next train (ms)
global startTime := "1710"        ; Default start time (5:10 PM in 24hr format)
global endTime := "1745"          ; Default end time (5:45 PM in 24hr format)

global workModeEnabled := false   ; Master switch: enables/disables the entire Work/Home mode system
global isWorkMode := false        ; Current active mode: false = Home mode, true = Work mode
global homeStation := "Lansdowne Road"  ; Station to monitor when in Home mode (commuting from work to home)
global workStation := "Pearse"    ; Station to monitor when in Work mode (commuting from home to work)
global homeDirection := "Northbound"    ; Train direction for Home mode (typically toward residential areas)
global workDirection := "Southbound"    ; Train direction for Work mode (typically toward city center)

; Map of all DART stations with user-friendly names and API names
global stations := Map(
    ; Northern Section
    "Malahide", "Malahide",
    "Portmarnock", "Portmarnock",
    "Clongriffin", "Clongriffin",
    "Howth Junction & Donaghmede", "Howth Junction",
    "Bayside", "Bayside",
    "Sutton", "Sutton",
    "Howth", "Howth",
    "Kilbarrack", "Kilbarrack",
    "Raheny", "Raheny",
    "Harmonstown", "Harmonstown",
    "Killester", "Killester",
    "Clontarf Road", "Clontarf Road",
    
    ; City Centre
    "Connolly", "Dublin Connolly",
    "Tara Street", "Tara Street",
    "Pearse", "Dublin Pearse",
    "Grand Canal Dock", "Grand Canal Dock",
    
    ; Southern Section
    "Lansdowne Road", "Lansdowne Road",
    "Sandymount", "Sandymount",
    "Sydney Parade", "Sydney Parade",
    "Booterstown", "Booterstown",
    "Blackrock", "Blackrock",
    "Seapoint", "Seapoint",
    "Salthill & Monkstown", "Salthill",
    "Dún Laoghaire", "Dun Laoghaire",
    "Sandycove & Glasthule", "Sandycove",
    "Glenageary", "Glenageary",
    "Dalkey", "Dalkey",
    "Killiney", "Killiney",
    "Shankill", "Shankill",
    "Bray", "Bray",
    "Greystones", "Greystones"
)

; System cursor management
global originalCursors := Map()
global currentCursorStatus := ""  ; Current cursor status (green, yellow, red)
global systemCursorIds := [32512, 32513, 32514, 32515, 32516, 32631, 32642, 32643, 32644, 32645, 32646, 32648, 32649, 32650, 32651, 32671, 32672]

; Cursor ID to filename mapping
; Thank you Ivan! 
global cursorDict := Map(
    32512, "arrow_eoa.cur",    ; IDC_ARROW
    32513, "ibeam_eoa.cur",    ; IDC_IBEAM
    32514, "wait_eoa.cur",     ; IDC_WAIT
    32515, "cross_eoa.cur",    ; IDC_CROSS
    32516, "up_eoa.cur",       ; IDC_UPARROW
    32631, "pen_eoa.cur",      ; IDC_HAND (pen in older Windows versions)
    32642, "nwse_eoa.cur",     ; IDC_SIZENWSE
    32643, "nesw_eoa.cur",     ; IDC_SIZENESW
    32644, "ew_eoa.cur",       ; IDC_SIZEWE
    32645, "ns_eoa.cur",       ; IDC_SIZENS
    32646, "move_eoa.cur",     ; IDC_SIZEALL
    32648, "unavail_eoa.cur",  ; IDC_NO
    32649, "link_eoa.cur",     ; IDC_HAND
    32650, "busy_eoa.cur",     ; IDC_APPSTARTING
    32651, "helpsel_eoa.cur",  ; IDC_HELP
    32671, "pin_eoa.cur",      ; IDB_PIN
    32672, "person_eoa.cur"    ; IDB_PERSON
)

; ===== Functions =====

; Helper function to sort an array alphabetically
SortArray(arr) {
    ; Create a new sorted array
    sorted := []
    
    ; Copy original array values
    for index, value in arr {
        sorted.Push(value)
    }
    
    ; Simple bubble sort
    n := sorted.Length
    Loop n {
        i := A_Index
        Loop (n - i) {
            j := A_Index
            ; Use StrCompare for string comparison instead of > operator
            if (StrCompare(sorted[j], sorted[j + 1]) > 0) {
                temp := sorted[j]
                sorted[j] := sorted[j + 1]
                sorted[j + 1] := temp
            }
        }
    }
    
    return sorted
}

; Helper function to sort an array alphabetically
SortArray(arr) {
    ; Create a new sorted array
    sorted := []
    
    ; Copy original array values
    for index, value in arr {
        sorted.Push(value)
    }
    
    ; Simple bubble sort
    n := sorted.Length
    Loop n {
        i := A_Index
        Loop (n - i) {
            j := A_Index
            ; Use StrCompare for string comparison instead of > operator
            if (StrCompare(sorted[j], sorted[j + 1]) > 0) {
                temp := sorted[j]
                sorted[j] := sorted[j + 1]
                sorted[j + 1] := temp
            }
        }
    }
    
    return sorted
}

; Function to initialize the script
InitializeScript() {
    global configFile, logFile, preferredDirection, currentStation, minCatchableTime
    global yellowThreshold, redDisplayTime, startTime, endTime, isActive, checkFrequency
    global workModeEnabled, isWorkMode, homeStation, workStation, homeDirection, workDirection
    
    ; Create or clear log file safely
    if (FileExist(logFile)) {
        try {
            FileDelete(logFile)
        } catch as e {
            ; If we can't delete the file, create a new one with a timestamp
            logFile := A_ScriptDir "\DartCursor_" . FormatTime(, "yyyyMMdd_HHmmss") . ".log"
        }
    }
    
    ; Create a fresh log file 
    try {
        FileAppend("=== Dart Cursor Log Started " . FormatTime(, "yyyy-MM-dd HH:mm:ss") . " ===`n", logFile)
    } catch {
        ; If we can't write to the log file, try an alternative location
        logFile := A_Temp "\DartCursor.log"
        FileAppend("=== Dart Cursor Log Started " . FormatTime(, "yyyy-MM-dd HH:mm:ss") . " ===`n", logFile)
    }
    
    LogMessage("Dart Cursor script starting")
    
    ; Back up the original system cursors
    backupCount := BackupSystemCursors()
    LogMessage("Backed up " . backupCount . " system cursors")
    
    ; Check if this is the first run by looking for configuration file
    isFirstRun := !FileExist(configFile)
      ; Load the configuration if it exists
    if (FileExist(configFile)) {
        preferredDirection := IniRead(configFile, "Settings", "Direction", "")
        currentStation := IniRead(configFile, "Settings", "Station", "Lansdowne Road")
        LogMessage("Loaded configuration. Station: " . currentStation . ", Direction: " . (preferredDirection ? preferredDirection : "Not set"))
        
        ; Load time thresholds
        minCatchableTime := Integer(IniRead(configFile, "Settings", "MinCatchableTime", "5"))
        yellowThreshold := Integer(IniRead(configFile, "Settings", "YellowThreshold", "5"))
        redDisplayTime := Integer(IniRead(configFile, "Settings", "RedDisplayTime", "60")) * 1000
        checkFrequency := Integer(IniRead(configFile, "Settings", "CheckFrequency", "60")) * 1000
          ; Load start and end times
        startTime := IniRead(configFile, "Settings", "StartTime", "1710")
        endTime := IniRead(configFile, "Settings", "EndTime", "1745")
        
        LogMessage("Loaded time thresholds - Min catchable: " . minCatchableTime . ", Yellow threshold: " . yellowThreshold . ", Red display time: " . (redDisplayTime/1000) . "s, Check frequency: " . (checkFrequency/1000) . "s")
        LogMessage("Loaded operational times - Start: " . FormatHHMM(startTime) . ", End: " . FormatHHMM(endTime))
          ; Load Work/Home mode settings with defaults
        workModeEnabled := IniRead(configFile, "Settings", "WorkModeEnabled", "false") = "true"
        isWorkMode := IniRead(configFile, "Settings", "IsWorkMode", "false") = "true"
        homeStation := IniRead(configFile, "Settings", "HomeStation", "Lansdowne Road")
        workStation := IniRead(configFile, "Settings", "WorkStation", "Pearse")
        homeDirection := IniRead(configFile, "Settings", "HomeDirection", "Northbound")
        workDirection := IniRead(configFile, "Settings", "WorkDirection", "Southbound")
        
        LogMessage("Loaded Work/Home mode settings - Enabled: " . (workModeEnabled ? "true" : "false") . ", Current mode: " . (isWorkMode ? "Work" : "Home"))
        
        ; Apply current Work/Home mode to active monitoring variables
        if (workModeEnabled) {
            UpdateStationForMode()
            LogMessage("Work/Home mode enabled - Current mode: " . (isWorkMode ? "Work" : "Home") . " - Station: " . currentStation)
        }
          ; Check if configuration is missing any essential values
        if (!IsConfigurationComplete()) {
            LogMessage("Configuration is incomplete - prompting for setup")
            ShowFirstRunWelcome()
        } else if (preferredDirection = "") {
            ; Open settings GUI directly instead of prompting for direction
            LogMessage("No direction selected - opening settings GUI")
            OpenSettingsGUI()
        }    } else {
        LogMessage("No configuration file found - First run detected")
        ; Show welcome message and prompt for initial setup
        ShowFirstRunWelcome()
    }    ; Set up tray menu
    InitializeTrayMenu()
    LogMessage("Tray menu initialized")
    
    ; Set up a periodic safety check to ensure cursors are restored when inactive
    SetTimer(EnsureCursorsRestored, 60000)  ; Check every minute
    
    ; Continue with time-based activation
    CheckStartTime()
}

; Function to show first-run welcome and setup wizard
ShowFirstRunWelcome() {
    global
    
    welcomeText := "Welcome to DART Cursor!`n`n"
    welcomeText .= "This tool changes your cursor colors based on upcoming DART train times.`n`n"
    welcomeText .= "• GREEN cursor: You have plenty of time to catch your train`n"
    welcomeText .= "• YELLOW cursor: You should leave soon`n"
    welcomeText .= "• RED cursor: You won't make it to this train`n`n"
    welcomeText .= "Let's set up your station and preferences now."
    
    MsgBox(welcomeText, "Welcome to DART Cursor", "Icon!")
    
    ; Open the settings GUI for initial configuration
    OpenSettingsGUI()
}

; Function to format HHMM string to HH:MM for display
FormatHHMM(timeString) {
    if (StrLen(timeString) = 4)
        return SubStr(timeString, 1, 2) . ":" . SubStr(timeString, 3, 2)
    return timeString
}

; Note: The PromptForDirection function has been removed as direction is now set via the Settings GUI

; Function to save configuration to a file
SaveConfig() {
    global preferredDirection, currentStation, minCatchableTime, yellowThreshold, redDisplayTime, startTime, endTime, checkFrequency
    global workModeEnabled, isWorkMode, homeStation, workStation, homeDirection, workDirection
    IniWrite(preferredDirection, configFile, "Settings", "Direction")
    IniWrite(currentStation, configFile, "Settings", "Station")
    IniWrite(minCatchableTime, configFile, "Settings", "MinCatchableTime")
    IniWrite(yellowThreshold, configFile, "Settings", "YellowThreshold")
    IniWrite(redDisplayTime/1000, configFile, "Settings", "RedDisplayTime") ; Store in seconds
    IniWrite(checkFrequency/1000, configFile, "Settings", "CheckFrequency") ; Store in seconds
    IniWrite(startTime, configFile, "Settings", "StartTime")
    IniWrite(endTime, configFile, "Settings", "EndTime")    ; Save Work/Home mode settings (convert booleans to strings explicitly)
    IniWrite(workModeEnabled ? "true" : "false", configFile, "Settings", "WorkModeEnabled")
    IniWrite(isWorkMode ? "true" : "false", configFile, "Settings", "IsWorkMode")
    IniWrite(homeStation, configFile, "Settings", "HomeStation")
    IniWrite(workStation, configFile, "Settings", "WorkStation")
    IniWrite(homeDirection, configFile, "Settings", "HomeDirection")
    IniWrite(workDirection, configFile, "Settings", "WorkDirection")
    
    LogMessage("Saved Work/Home mode settings - Enabled: " . (workModeEnabled ? "true" : "false") . ", Current mode: " . (isWorkMode ? "Work" : "Home"))
}

; Function to allow changing direction from tray menu
ChangeDirection(*) {
    ; Open the settings GUI instead of using the old PromptForDirection function
    OpenSettingsGUI()
}

; Function to allow changing station from tray menu
ChangeStation(*) {
    global currentStation, stations, isActive
    
    ; Create a GUI with dropdown and autocomplete
    stationPickGui := Gui("+AlwaysOnTop", "Select DART Station")
    
    ; Add instruction text
    stationPickGui.AddText("w380", "Select your DART station (current: " . currentStation . "):")
    
    ; Create an array of station names for the dropdown
    stationList := []
    for stationName, apiName in stations {
        stationList.Push(stationName)
    }
    
    ; Sort the station names alphabetically
    stationList := SortArray(stationList)
    
    ; Add the ComboBox with dropdown list style
    stationCombo := stationPickGui.AddComboBox("w380")  ; Simple dropdown
    
    ; Add items to the ComboBox
    for i, stationName in stationList {
        stationCombo.Add([stationName])
    }
      ; Try to select current station in the dropdown
    currentDisplayName := ""
    currentIndex := 0
    index := 1
    
    for stationName, apiName in stations {
        if (apiName = currentStation) {
            currentDisplayName := stationName
            currentIndex := index
            break
        }
        index++
    }
    
    if (currentIndex > 0)
        stationCombo.Value := currentIndex
    
    ; Add OK and Cancel buttons
    stationPickGui.AddButton("w100 x170 y+20 Default", "Select").OnEvent("Click", SelectStation)
    stationPickGui.AddButton("w100 x+10 y+-25", "Cancel").OnEvent("Click", (*) => stationPickGui.Destroy())
    
    ; Function to handle station selection
    SelectStation(*) {
        ; Get the selected text from the combo box
        selectedIndex := stationCombo.Value
        selectedStation := stationList[selectedIndex]  ; Get station name by index
          ; Check if the selected station exists
        if (stations.Has(selectedStation)) {
            apiStation := stations[selectedStation]
            
            ; Update the station
            currentStation := apiStation
            LogMessage("Station changed to: " . selectedStation . " (API name: " . currentStation . ")")
            
            ; Save to config
            SaveConfig()
            
            ; Force an update
            if (isActive)
                CheckNow()
            
            ; Close the GUI
            stationPickGui.Destroy()
        } else {
            ; No exact match found, warn user
            MsgBox("'" . selectedStation . "' is not a recognized DART station. Please select a station from the dropdown list.", 
                   "Invalid Station", "Icon!")
        }
    }
    
    ; Show the GUI
    stationPickGui.Show()
}

; Function to set the minimum catchable time
SetCatchableTime(*) {
    global minCatchableTime
    
    userInput := InputBox("Enter minimum minutes needed to catch a train (current: " . minCatchableTime . "):", 
                        "Minimum Catchable Time", "w300 h120", minCatchableTime)
      if (userInput.Result = "OK") {
        newTime := Integer(userInput.Value)
        if (newTime >= 0 && newTime <= 30) {
            minCatchableTime := newTime
            LogMessage("Minimum catchable time set to " . minCatchableTime . " minutes")
            ; Save to config
            IniWrite(minCatchableTime, configFile, "Settings", "MinCatchableTime")
            
            ; Force an update
            if (isActive)
                CheckNow()
        } else {
            MsgBox("Please enter a number between 0 and 30", "Invalid Input", "Iconi")
        }
    }
}

; Function to set the yellow threshold time
SetYellowThreshold(*) {
    global yellowThreshold, minCatchableTime
    
    userInput := InputBox("Enter minutes until train arrival for yellow warning (current: " . yellowThreshold . "):" 
                        . "`n`nNote: Yellow warning will show when train is due between " 
                        . yellowThreshold . " and " . minCatchableTime . " minutes."
                        . "`nThis value must be greater than the minimum catchable time (" . minCatchableTime . ").", 
                        "Yellow Warning Threshold", "w400 h160", yellowThreshold)
      if (userInput.Result = "OK") {
        newTime := Integer(userInput.Value)
        if (newTime > minCatchableTime && newTime <= 30) {
            yellowThreshold := newTime
            LogMessage("Yellow threshold set to " . yellowThreshold . " minutes")
            ; Save to config
            IniWrite(yellowThreshold, configFile, "Settings", "YellowThreshold")
            
            ; Force an update
            if (isActive)
                CheckNow()
        } else {
            MsgBox("Please enter a number greater than the minimum catchable time (" 
                  . minCatchableTime . ") and no more than 30.", "Invalid Input", "Iconi")
        }
    }
}

; Function to set the red display time
SetRedDisplayTime(*) {
    global redDisplayTime
    
    userInput := InputBox("Enter seconds to display red cursor before moving to next train (current: " . (redDisplayTime/1000) . "):", 
                        "Red Display Time", "w300 h120", redDisplayTime/1000)
      if (userInput.Result = "OK") {
        newSeconds := Integer(userInput.Value)
        if (newSeconds >= 10 && newSeconds <= 300) {
            redDisplayTime := newSeconds * 1000
            LogMessage("Red display time set to " . newSeconds . " seconds")
            ; Save to config
            IniWrite(newSeconds, configFile, "Settings", "RedDisplayTime")
            
            ; Force an update
            if (isActive)
                CheckNow()
        } else {
            MsgBox("Please enter a number between 10 and 300 seconds", "Invalid Input", "Iconi")
        }
    }
}

; Function to set the start time
SetStartTime(*) {
    global startTime
    
    ; Format current start time for display
    currentDisplayTime := FormatHHMM(startTime)
    
    userInput := InputBox("Enter script start time in 24-hour format (HH:MM) (current: " . currentDisplayTime . "):", 
                       "Set Start Time", "w300 h120", currentDisplayTime)
    
    if (userInput.Result = "OK") {
        ; Validate and process input
        newTime := userInput.Value
        
        ; Check if the format is valid (HH:MM)
        if (RegExMatch(newTime, "^([01]\d|2[0-3]):([0-5]\d)$")) {
            ; Convert from HH:MM to HHMM format for storage
            newTimeFormatted := StrReplace(newTime, ":", "")
            global startTime := newTimeFormatted
            LogMessage("Start time set to " . FormatHHMM(startTime))
            
            ; Save to config
            SaveConfig()
            
            ; Update timer/checking
            CheckStartTime()
        } else {
            MsgBox("Please enter time in valid 24-hour format (HH:MM)", "Invalid Format", "Iconi")
        }
    }
}

; Function to set the end time
SetEndTime(*) {
    global endTime
    
    ; Format current end time for display
    currentDisplayTime := FormatHHMM(endTime)
    
    userInput := InputBox("Enter script end time in 24-hour format (HH:MM) (current: " . currentDisplayTime . "):", 
                       "Set End Time", "w300 h120", currentDisplayTime)
    
    if (userInput.Result = "OK") {
        ; Validate and process input
        newTime := userInput.Value
        
        ; Check if the format is valid (HH:MM)
        if (RegExMatch(newTime, "^([01]\d|2[0-3]):([0-5]\d)$")) {
            ; Convert from HH:MM to HHMM format for storage
            newTimeFormatted := StrReplace(newTime, ":", "")
            global endTime := newTimeFormatted
            LogMessage("End time set to " . FormatHHMM(endTime))
            
            ; Save to config
            SaveConfig()
            
            ; Update timer/checking
            CheckStartTime()
        } else {
            MsgBox("Please enter time in valid 24-hour format (HH:MM)", "Invalid Format", "Iconi")
        }
    }
}

; Function to log messages to the log file with retry mechanism
LogMessage(message) {
    timestamp := FormatTime(, "yyyy-MM-dd HH:mm:ss")
    logText := timestamp . " - " . message . "`n"
    
    ; Try to write to the log file with retries
    Loop 3 {
        try {
            FileAppend(logText, logFile)
            return  ; Success
        } catch as e {
            if (A_Index < 3) {
                ; Wait a bit before retrying
                Sleep(50)
            } else {
                ; If all retries fail, write to a backup log or display a message
                backupLog := A_ScriptDir "\DartCursor_backup.log"
                try {
                    FileAppend("BACKUP LOG - " . logText, backupLog)
                } catch {
                    ; If even backup logging fails, add to tray tip
                    TrayTip("Logging Error", "Could not write to log file: " . message, 3)
                }
            }
        }
    }
}

; Function to check if it's time to start monitoring
CheckStartTime() {
    global startTime, endTime, isActive
    currentTime := FormatTime(, "HHmm")
    currentHour := SubStr(currentTime, 1, 2)
    currentDate := FormatTime(, "yyyyMMdd")
    
    ; Format times for display in logs
    displayStartTime := FormatHHMM(startTime)
    displayEndTime := FormatHHMM(endTime)
    
    ; If it's between configured start and end times, start checking trains
    if (currentTime >= startTime && currentTime < endTime) {
        ; Only log and activate if we weren't already active
        if (!isActive) {
            isActive := true
            LogMessage("Starting train checks at " . FormatTime(, "HH:mm:ss"))
            StartChecking()
        }
    } else {
        ; Outside active hours (either before start time or after end time)
        ; Get current time display for logging
        displayCurrentTime := FormatTime(, "HH:mm:ss")
        
        ; Deactivate if active and restore cursors
        if (isActive) {
            isActive := false
            LogMessage("Stopping train checks at " . displayCurrentTime . " (outside active hours " . displayStartTime . "-" . displayEndTime . ")")
            
            ; IMPORTANT: Stop all timers that might restart activity
            SetTimer(CheckNow, 0)  ; Stop the main check timer
            SetTimer(MoveToNextTrain, 0)  ; Stop any red cursor timers
            
            ; Always restore system cursors when deactivating
            RestoreSystemCursors()  ; Restore normal cursors
            
            ; No longer exiting script, just deactivating
            TrayTip("DART Cursor", "Monitoring stopped. Will resume at " . displayStartTime . " when in active hours.", 3)
        } else {
            ; Script is already inactive, ensure cursors are restored
            LogMessage("Outside active hours (" . displayStartTime . "-" . displayEndTime . ") - waiting for next start time, current time: " . displayCurrentTime)
            RestoreSystemCursors()  ; Ensure cursors are restored
        }
        
        ; If it's getting close to start time (within 10 minutes), check more frequently
        startHour := Integer(SubStr(startTime, 1, 2))
        startMinute := Integer(SubStr(startTime, 3, 2))
        currentHour := Integer(SubStr(currentTime, 1, 2))
        currentMinute := Integer(SubStr(currentTime, 3, 2))
        
        timeToStartMinutes := (startHour - currentHour) * 60 + (startMinute - currentMinute)
        if (timeToStartMinutes < 0) {
            timeToStartMinutes += 1440  ; Add 24 hours (next day)
        }
        
        if (timeToStartMinutes <= 10) {
            SetTimer(CheckStartTime, 30000)  ; Check every 30 seconds
        } else {
            SetTimer(CheckStartTime, 60000)  ; Check every minute
        }
    }
}

; Function to start the check timer
StartChecking() {
    ; Check immediately
    CheckNow()
    ; Set up the timer for regular checks
    SetTimer(CheckNow, checkFrequency)
}

; Function to check train times now (and called by the timer)
CheckNow(*) {
    ; First check if we're still within the active time window
    global startTime, endTime, isActive
    currentTime := FormatTime(, "HHmm")
    
    ; Check if we've reached or passed the end time
    if (currentTime >= endTime || currentTime < startTime) {
        ; We're outside the active window, deactivate if needed
        if (isActive) {
            isActive := false
            LogMessage("End time reached during check - stopping train checks")
            SetTimer(CheckNow, 0)  ; Stop the timer
            RestoreSystemCursors()  ; Restore normal cursors
            TrayTip("DART Cursor", "Monitoring stopped - outside active hours", 3)
            
            ; Set a timer to periodically check if we're back in the active time window
            SetTimer(CheckStartTime, 60000)  ; Check every minute
            return
        }
        return  ; Don't fetch train data if we're outside the active window
    }
    
    ; Only fetch train data if we're in the active window
    FetchTrainData()
}

; Function to fetch train data from the Irish Rail API
FetchTrainData() {
    try {
        LogMessage("Fetching train data for station: " . currentStation)
        
        ; Reset train tracking index when fetching new data
        global currentTrainIndex := 1
        
        ; Build the API URL
        apiUrl := "https://api.irishrail.ie/realtime/realtime.asmx/getStationDataByNameXML?StationDesc=" currentStation
        LogMessage("API URL: " . apiUrl)
        
        ; Make HTTP request
        whr := ComObject("WinHttp.WinHttpRequest.5.1")
        whr.Open("GET", apiUrl, false)
        whr.SetRequestHeader("User-Agent", "Mozilla/5.0")
        whr.SetRequestHeader("Accept", "text/xml")
        whr.Send()
        response := whr.ResponseText
          ; Check if we're getting any data
        if (response = "" || !InStr(response, "<")) {
            LogMessage("Error: Empty or invalid response from API")
            ToolTip("Error: Empty or invalid response from API")
            return
        }
        
        ; Log response preview for debugging
        LogMessage("API response received, length: " . StrLen(response) . ", preview: " . SubStr(response, 1, 100))
        
        ; Create XML document
        xml := ComObject("MSXML2.DOMDocument.6.0")
        xml.async := false
        xml.setProperty("SelectionLanguage", "XPath")
          ; Load the XML and check for errors
        if (!xml.loadXML(response)) {
            errorMsg := "Error parsing XML: " . xml.parseError.reason
            LogMessage(errorMsg)
            ToolTip(errorMsg)
            return
        }
        
        ; Try multiple XPath patterns to find train data nodes
        trains := xml.selectNodes("//objStationData")
        
        if (!trains || trains.length = 0) {
            trains := xml.selectNodes("//ArrayOfObjStationData/objStationData")
        }
        
        if (!trains || trains.length = 0) {
            trains := xml.selectNodes("//*[local-name()='objStationData']")
        }
        
        if (!trains || trains.length = 0) {
            trains := xml.selectNodes("/*[local-name()='ArrayOfObjStationData']/*[local-name()='objStationData']")
        }
          if (!trains || trains.length = 0) {
            LogMessage("No trains found in XML response")
            ToolTip("No trains found.")
            return
        }
        
        LogMessage("Found " . trains.length . " trains in XML data")
          ; Collect train data for processing
        global trainData := []
        
        Loop trains.length {
            train := trains.item(A_Index - 1)  ; Zero-based index for XML nodes            ; Extract data with fallbacks for different XML structures
            direction := ExtractXmlNode(train, "Direction", "Unknown")
            destination := ExtractXmlNode(train, "Destination", "Unknown")
            dueIn := ExtractXmlNode(train, "Duein", "999")
            
            ; Log the train data for debugging
            LogMessage("Found train: Direction=" . direction . ", Destination=" . destination . ", DueIn=" . dueIn)
            
            ; Add trains based on preferred direction
            ; If no direction is set, add all trains
            if (preferredDirection = "" || (direction && InStr(direction, preferredDirection))) {
                trainData.Push({due: Integer(dueIn), direction: direction, 
                              destination: destination, dueText: dueIn " min"})
                LogMessage("Added train to list: " . destination . " in " . dueIn . " minutes")
            }
        }
        
        ; Sort trains by due time (ascending)
        trainData := SortByDueTime(trainData)
        
        ; Update cursor based on next train time
        UpdateCursor()
      } catch as e {
        errorMsg := "Error: " . e.Message
        LogMessage(errorMsg)
        LogMessage("Stack: " . e.Stack)
        ToolTip(errorMsg)
    }
}

; Function to extract XML node values with fallbacks
ExtractXmlNode(node, nodeName, defaultValue) {
    try {
        value := node.selectSingleNode(nodeName).text
        return value ? value : defaultValue
    } catch {
        try {
            value := node.selectSingleNode("*[local-name()='" nodeName "']").text
            return value ? value : defaultValue
        } catch {
            return defaultValue
        }
    }
}

; Function to sort trains by due time
SortByDueTime(trains) {
    ; Simple bubble sort
    n := trains.Length
    Loop n {
        i := A_Index
        Loop (n - i) {
            j := A_Index
            if (trains[j].due > trains[j + 1].due) {
                temp := trains[j]
                trains[j] := trains[j + 1]
                trains[j + 1] := temp
            }
        }
    }
    return trains
}

; Function to get cursor color based on time until train arrival
GetCursorColor(minutesUntilArrival) {
    global minCatchableTime, yellowThreshold
    
    if (minutesUntilArrival <= minCatchableTime) {
        ; Red - the train is arriving at or below the minimum catchable time
        return 0xFF0000 ; Red
    } else if (minutesUntilArrival <= yellowThreshold) {
        ; Yellow - getting close to minimum time
        return 0xFFFF00 ; Yellow
    } else {
        ; Green - plenty of time
        return 0x00FF00 ; Green
    }
    ; Note: This function is kept for reference but the actual color logic is in UpdateCursor()
}

; Function to update the cursor color based on next catchable train
UpdateCursor() {
    if (trainData.Length = 0) {
        LogMessage("No trains found in preferred direction. Restoring original cursors.")
        RestoreSystemCursors()
        return
    }
    
    global currentTrainIndex, minCatchableTime, yellowThreshold, redDisplayTime
    
    ; Reset train index if we're beyond the array bounds
    if (currentTrainIndex > trainData.Length) {
        currentTrainIndex := 1
    }
    
    ; Check if the current train is too close to catch
    if (currentTrainIndex = 1 && trainData[1].due < minCatchableTime) {
        ; Current train is too close - find the next catchable train
        LogMessage("Train " . trainData[1].destination . " arriving in " . trainData[1].due . " minutes is too close to catch.")
        
        ; Look for the next train
        if (trainData.Length > 1) {
            currentTrainIndex := 2
            LogMessage("Skipping to next train: " . trainData[2].destination . " in " . trainData[2].due . " minutes")
        }
    }
      ; Get the selected train's due time
    nextTrainDue := trainData[currentTrainIndex].due    ; Determine cursor status based on arrival time according to specified rules:
    ; Green by default
    ; Yellow when train is arriving between yellowThreshold and minCatchableTime
    ; Red when train is arriving in less than minCatchableTime (for redDisplayTime seconds)
    cursorStatus := ""
    if (nextTrainDue <= minCatchableTime) {
        ; Red - train arriving very soon (below minimum catchable time)
        cursorStatus := "red"
        LogMessage("Selected train due in " . nextTrainDue . " minutes (below minimum catchable time). Setting RED cursors.")
          ; Set timer to move to next train after redDisplayTime milliseconds
        if (trainData.Length > currentTrainIndex + 1) {
            SetTimer(MoveToNextTrain, redDisplayTime)
            LogMessage("Red cursor timer set for " . (redDisplayTime/1000) . " seconds")
        }
    } else if (nextTrainDue <= yellowThreshold) {
        ; Yellow - train arriving soon but still catchable
        cursorStatus := "yellow"
        LogMessage("Selected train due in " . nextTrainDue . " minutes. Setting YELLOW cursors.")
        
        ; Clear any red timer if it exists
        SetTimer(MoveToNextTrain, 0)
    } else {
        ; Green - plenty of time (default)
        cursorStatus := "green"
        LogMessage("Selected train due in " . nextTrainDue . " minutes. Setting GREEN cursors.")
        
        ; Clear any red timer if it exists
        SetTimer(MoveToNextTrain, 0)
    }
    
    ; Update all system cursors based on status
    SetSystemCursors(cursorStatus)
      ; Show tooltip with next train info and station name
    dirText := preferredDirection ? preferredDirection : "Next"
    trainInfo := "Station: " . currentStation . " | " . dirText . " catchable train: " . trainData[currentTrainIndex].destination . " in " . trainData[currentTrainIndex].dueText
    
    ; If we're skipping a train, include that info
    if (currentTrainIndex > 1) {
        trainInfo .= " (Skipped train in " . trainData[1].dueText . ")"
    }
    
    ToolTip(trainInfo)
    LogMessage("Showing tooltip: " . trainInfo)
    SetTimer(() => ToolTip(), -3000) ; Hide tooltip after 3 seconds
}

; Function to move to the next train when a red cursor timer expires
MoveToNextTrain(*) {
    global currentTrainIndex, trainData
    
    ; Make sure we have at least one more train available
    if (trainData.Length > currentTrainIndex + 1) {
        ; Move to the next train
        currentTrainIndex += 1
        LogMessage("Red timer expired - Moving to next train: " . trainData[currentTrainIndex].destination . " in " . trainData[currentTrainIndex].dueText)
        
        ; Update the cursor based on the new train
        UpdateCursor()
    } else {
        LogMessage("Red timer expired - No more trains available, staying on the current train")
    }
    
    return true
}

; The GetCursorColor function is retained but is no longer used with the overlay
; It's kept for reference if needed later

; Function to backup the original system cursors
BackupSystemCursors() {
    global originalCursors, systemCursorIds
    
    ; Clear the existing backup
    originalCursors := Map()
    
    ; For each system cursor type, create a backup
    for cursorId in systemCursorIds {
        ; Get the cursor handle
        cursorHandle := DllCall("LoadCursor", "Ptr", 0, "Ptr", cursorId, "Ptr")
        
        ; Copy it for our backup
        if (cursorHandle) {
            ; We're just storing the system ID, as we'll restore using LoadCursor
            originalCursors[cursorId] := cursorId
        }
    }
    
    LogMessage("Original system cursors backed up")
    return originalCursors.Count
}

; Function to set the system cursors based on a status
SetSystemCursors(status) {
    global cursorsFolderPath, cursorDict, currentCursorStatus
    
    ; If status is the same as current, no need to change
    if (status = currentCursorStatus) {
        return true
    }
    
    ; Save the new status
    currentCursorStatus := status
      ; Determine which folder to use based on status
    if (status = "green") {
        cursorsFolderPath := A_ScriptDir . "\inrange_cursors\"
        LogMessage("Setting GREEN cursors (plenty of time)")
    } else if (status = "ffbb00") {
        cursorsFolderPath := A_ScriptDir . "\orange_cursors\"
        LogMessage("Setting ORANGE cursors (leave soon)")
    } else if (status = "red") {
        cursorsFolderPath := A_ScriptDir . "\low_cursors\"
        LogMessage("Setting RED cursors (won't make it)")
    } else {
        LogMessage("Invalid cursor status: " . status)
        return false
    }
    
    ; Make sure the folder exists
    if (!DirExist(cursorsFolderPath)) {
        LogMessage("Cursor folder not found: " . cursorsFolderPath)
        return false
    }
    
    ; Count for logging purposes
    successCount := 0
    
    ; Set each system cursor
    for cursorId, cursorName in cursorDict {
        cursorPath := cursorsFolderPath . cursorName
        
        ; Check if the cursor file exists
        if (!FileExist(cursorPath)) {
            LogMessage("Cursor file not found: " . cursorPath)
            continue
        }
        
        ; Load and set the custom cursor
        try {
            hCursor := DllCall("LoadCursorFromFile", "Str", cursorPath, "Ptr")
            
            if (hCursor) {
                ; Set the system cursor
                result := DllCall("SetSystemCursor", "Ptr", DllCall("CopyImage", "Ptr", hCursor, 
                                 "UInt", 2, "Int", 0, "Int", 0, "UInt", 0, "Ptr"), 
                                 "UInt", cursorId)
                
                if (result) {
                    successCount++
                } else {
                    LogMessage("Failed to set system cursor: " . cursorName . " (Error: " . A_LastError . ")")
                }
                
                ; Free loaded cursor
                DllCall("DestroyCursor", "Ptr", hCursor)
            } else {
                LogMessage("Failed to load cursor from file: " . cursorPath . " (Error: " . A_LastError . ")")
            }
        } catch as e {
            LogMessage("Exception when setting cursor: " . cursorName . " - " . e.Message)
        }
    }
    
    LogMessage("Successfully set " . successCount . " system cursors to " . status)
    return (successCount > 0)
}

; Function to restore the original system cursors
RestoreSystemCursors() {
    global originalCursors, systemCursorIds, currentCursorStatus
    
    if (originalCursors.Count = 0) {
        LogMessage("No cursor backup found to restore")
        
        ; If no backup exists, restore system defaults directly
        DllCall("SystemParametersInfo", "UInt", 0x57, "UInt", 0, "Ptr", 0, "UInt", 0)  ; SPI_SETCURSORS
        LogMessage("System default cursors restored")
        return true
    }
    
    ; Count for logging purposes
    successCount := 0
    
    ; Restore each system cursor from backup
    for cursorId in systemCursorIds {
        if (originalCursors.Has(cursorId)) {
            ; Restore this cursor from system defaults
            hCursor := DllCall("LoadCursor", "Ptr", 0, "Ptr", cursorId, "Ptr")
            
            if (hCursor) {
                ; Set the system cursor back to original
                result := DllCall("SetSystemCursor", "Ptr", DllCall("CopyImage", "Ptr", hCursor, 
                                 "UInt", 2, "Int", 0, "Int", 0, "UInt", 0, "Ptr"), 
                                 "UInt", cursorId)
                
                if (result) {
                    successCount++
                }
            }
        }
    }
    
    LogMessage("Restored " . successCount . " system cursors to original")
    
    ; Clear the backup
    originalCursors := Map()
    currentCursorStatus := ""
    
    return (successCount > 0)
}

; Function to restore default cursors from tray menu
RestoreDefaultCursors(*) {
    RestoreSystemCursors()
    TrayTip("DART Cursor", "Default system cursors restored", 3)
}

; Function to open a comprehensive settings GUI
OpenSettingsGUI(*) {
    ; Define GUI variables as global so they can be accessed in SaveSettings
    global currentStation, preferredDirection, minCatchableTime, yellowThreshold, redDisplayTime, startTime, endTime, stations
    global settingsGui, stationDropdown, directionDropdown, minCatchableEdit
    global yellowEdit, redDisplayEdit, startHourEdit, startMinEdit, endHourEdit, endMinEdit
    global stationNames
    
    ; Create GUI
    settingsGui := Gui("", "DART Cursor Settings")
    settingsGui.MarginX := 15
    settingsGui.MarginY := 15
    
    ; Add header text
    settingsGui.SetFont("s12 bold")
    headerText := settingsGui.AddText("w480 Center", "DART Cursor Settings")
    settingsGui.SetFont()
    
    settingsGui.AddText("w480 xm y+10", "Configure how the cursor changes color to notify you of approaching trains.")
    settingsGui.AddText("w480 xm y+5", "Set your preferred station, direction, and timing thresholds.")
    
    ; Create tab control with extra padding to prevent text clipping
    tabs := settingsGui.Add("Tab3", "w500 h400 +0x800000", ["Station", "Timing", "Schedule", "About"])
    
    ; ======= Station Tab =========
    tabs.UseTab(1)
    
    ; Station selection
    settingsGui.AddText("xm+10 y+25 w120", "Your Station:")    ; Create array of station names for the dropdown organized by geographic section
    global stationNames := []
    
    ; Add a blank entry at the start for visual separation
    stationNames.Push("")
    
    ; Add Northern Section with header and spacing
    stationNames.Push("=== NORTHERN SECTION ===")
    stationNames.Push("") ; Empty space after header
    northernStations := ["Malahide", "Portmarnock", "Clongriffin", "Howth Junction & Donaghmede", 
                         "Bayside", "Sutton", "Howth", "Kilbarrack", "Raheny", "Harmonstown", 
                         "Killester", "Clontarf Road"]
    for station in northernStations
        stationNames.Push(station)
    
    ; Add spacing before next section
    stationNames.Push("")
    stationNames.Push("")
    
    ; Add City Centre Section with header and spacing
    stationNames.Push("=== CITY CENTRE ===")
    stationNames.Push("") ; Empty space after header
    cityStations := ["Connolly", "Tara Street", "Pearse", "Grand Canal Dock"]
    for station in cityStations
        stationNames.Push(station)
    
    ; Add spacing before next section
    stationNames.Push("")
    stationNames.Push("")
    
    ; Add Southern Section with header and spacing
    stationNames.Push("=== SOUTHERN SECTION ===")
    stationNames.Push("") ; Empty space after header
    southernStations := ["Lansdowne Road", "Sandymount", "Sydney Parade", "Booterstown", 
                         "Blackrock", "Seapoint", "Salthill & Monkstown", "Dún Laoghaire", 
                         "Sandycove & Glasthule", "Glenageary", "Dalkey", "Killiney", 
                         "Shankill", "Bray", "Greystones"]
    for station in southernStations
        stationNames.Push(station)
    ; Find current station's display name and index in our organized list
    currentStationDisplay := ""
    selectedIndex := 1
    for i, stationName in stationNames {
        ; Skip section headers (they start with ---)
        if (SubStr(stationName, 1, 3) = "---")
            continue
            
        if (stations.Has(stationName) && stations[stationName] = currentStation) {
            currentStationDisplay := stationName
            selectedIndex := i
            break
        }
    }
      ; Add dropdown with section headers
    stationDropdown := settingsGui.AddDropDownList("w355 x+5 yp-3 Choose" . selectedIndex, stationNames)
    
    ; Change section header styles to be bold and non-selectable
    control := stationDropdown.Hwnd
    for i, item in stationNames {
        if (SubStr(item, 1, 3) = "---") {
            ; Set the item to bold (need Windows API calls)
            SendMessage(0x170, i-1, -1, control) ; CB_SETITEMDATA
        }
    }
    
    ; Direction selection
    settingsGui.AddText("xm+10 y+20 w120", "Direction:")
    directionDropdown := settingsGui.AddDropDownList("w355 x+5 yp-3", ["No Direction Filter", "Northbound", "Southbound"])
    
    ; Set current direction in dropdown
    if (preferredDirection = "Northbound")
        directionDropdown.Value := 2
    else if (preferredDirection = "Southbound")
        directionDropdown.Value := 3
    else
        directionDropdown.Value := 1    ; Add explanation 
    settingsGui.AddText("xm+10 y+10 w480", "Select your DART station and the direction you typically travel.")
    
    ; Work/Home Mode Section
    settingsGui.AddText("xm+10 y+15 w480 cBlue", "Work/Home Quick Switch:")
    workModeCheck := settingsGui.AddCheckbox("xm+10 y+5 w350 Checked" . (workModeEnabled ? "1" : "0"), "Enable Work/Home Mode (quick station switching)")
    
    ; Home station selection
    settingsGui.AddText("xm+10 y+10 w100", "Home Station:")
    homeStationDropdown := settingsGui.AddDropDownList("w180 x+5 yp-3", stationNames)
    settingsGui.AddText("x+5 yp+3 w60", "Direction:")
    homeDirectionDropdown := settingsGui.AddDropDownList("w70 x+5 yp-3", ["North", "South"])
    
    ; Work station selection  
    settingsGui.AddText("xm+10 y+10 w100", "Work Station:")
    workStationDropdown := settingsGui.AddDropDownList("w180 x+5 yp-3", stationNames)
    settingsGui.AddText("x+5 yp+3 w60", "Direction:")
    workDirectionDropdown := settingsGui.AddDropDownList("w70 x+5 yp-3", ["North", "South"])
    
    ; Set current stations in dropdowns
    for i, stationName in stationNames {
        if (SubStr(stationName, 1, 3) = "---")
            continue
        if (stations.Has(stationName) && stations[stationName] = homeStation) {
            homeStationDropdown.Value := i
        }
        if (stations.Has(stationName) && stations[stationName] = workStation) {
            workStationDropdown.Value := i
        }
    }
    
    ; Set directions
    homeDirectionDropdown.Value := (homeDirection = "Southbound") ? 2 : 1
    workDirectionDropdown.Value := (workDirection = "Southbound") ? 2 : 1
    
    ; Current mode display
    if (workModeEnabled) {
        currentModeText := settingsGui.AddText("xm+10 y+15 w480 cRed", "Current Mode: " . (isWorkMode ? "WORK" : "HOME") . " - Use tray menu to switch")
    } else {
        settingsGui.AddText("xm+10 y+15 w480", "Enable Work/Home mode to quickly switch between two preset stations via tray menu.")
    }
    
    ; ======= Timing Tab =========
    tabs.UseTab(2)
    
    ; Min catchable time
    settingsGui.AddText("xm+10 y+25 w170", "Minimum Catchable Time:")
    minCatchableEdit := settingsGui.AddEdit("w80 x+10 yp-3 Number", minCatchableTime)
    settingsGui.AddText("x+10 yp+3 w200", "minutes (cursor turns RED)")
    settingsGui.AddText("xm+10 y+3 w450", "When a train is this close or less, it's too late to catch it (RED cursor)")
    
    ; Yellow threshold
    settingsGui.AddText("xm+10 y+20 w170", "Yellow Warning Threshold:")
    yellowEdit := settingsGui.AddEdit("w80 x+10 yp-3 Number", yellowThreshold)
    settingsGui.AddText("x+10 yp+3 w200", "minutes (cursor turns YELLOW)")
    settingsGui.AddText("xm+10 y+3 w450", "When train is due between yellow threshold and min catchable time (YELLOW cursor)")
    
    ; Visual explanation of thresholds
    settingsGui.AddText("xm+10 y+15 w480 cBlue", "How Thresholds Work:")
    settingsGui.AddText("xm+10 y+5 w480", "• Train due in > " . yellowThreshold . " minutes: GREEN cursor (plenty of time)")
    settingsGui.AddText("xm+10 y+5 w480", "• Train due in " . minCatchableTime . "-" . yellowThreshold . " minutes: YELLOW cursor (leave soon)")
    settingsGui.AddText("xm+10 y+5 w480", "• Train due in < " . minCatchableTime . " minutes: RED cursor (won't make it)")
      ; Red display time
    settingsGui.AddText("xm+10 y+20 w170", "Red Display Time:")
    redDisplayEdit := settingsGui.AddEdit("w80 x+10 yp-3 Number", redDisplayTime/1000)
    settingsGui.AddText("x+10 yp+3 w200", "seconds")
    settingsGui.AddText("xm+10 y+3 w450", "How long to show red cursor before moving to next train")
      ; API Check Frequency
    settingsGui.AddText("xm+10 y+20 w170", "API Check Frequency:")
    checkFrequencyEdit := settingsGui.AddEdit("w80 x+10 yp-3 Number", checkFrequency/1000)
    settingsGui.AddText("x+10 yp+3 w200", "seconds")
    settingsGui.AddText("xm+10 y+3 w450", "How often to check for new train data from the DART API (10-600 seconds)")
    settingsGui.AddText("xm+10 y+5 w450", "Lower values = more responsive updates, Higher values = less network usage")
    
    ; ======= Schedule Tab =========
    tabs.UseTab(3)
    
    ; Format times for display
    startTimeDisplay := FormatHHMM(startTime)
    endTimeDisplay := FormatHHMM(endTime)
    
    settingsGui.AddText("xm+10 y+25 w480", "The script will only check for trains and change cursors between these times.")
    settingsGui.AddText("xm+10 y+5 w480", "Outside these hours, your cursors will remain at normal system defaults.")
    
    ; Start time selection
    settingsGui.AddText("xm+10 y+20 w120", "Start Time:")
    startHourEdit := settingsGui.AddEdit("w40 x+10 yp-3 Number Limit2", SubStr(startTimeDisplay, 1, 2))
    settingsGui.AddText("x+5 yp+3 w10", ":")
    startMinEdit := settingsGui.AddEdit("w40 x+5 yp-3 Number Limit2", SubStr(startTimeDisplay, 4, 2))
    settingsGui.AddText("x+10 yp+3 w250", "(24-hour format)")
    
    ; End time selection
    settingsGui.AddText("xm+10 y+20 w120", "End Time:")
    endHourEdit := settingsGui.AddEdit("w40 x+10 yp-3 Number Limit2", SubStr(endTimeDisplay, 1, 2))
    settingsGui.AddText("x+5 yp+3 w10", ":")
    endMinEdit := settingsGui.AddEdit("w40 x+5 yp-3 Number Limit2", SubStr(endTimeDisplay, 4, 2))
    settingsGui.AddText("x+10 yp+3 w250", "(24-hour format)")
    
    ; Examples
    settingsGui.AddText("xm+10 y+15 w480 cBlue", "Examples:")
    settingsGui.AddText("xm+10 y+5 w480", "• For evening commute: Start 17:00, End 18:30")
    settingsGui.AddText("xm+10 y+5 w480", "• For morning commute: Start 07:30, End 09:00")
    
    ; ======= About Tab =========
    tabs.UseTab(4)
    
    settingsGui.AddText("xm+10 y+25 w480", "DART Cursor - Visual Train Time Notification System")
    settingsGui.AddText("xm+10 y+10 w480", "This tool changes your Windows cursor colors based on upcoming DART train times at your selected station.")
    
    settingsGui.AddText("xm+10 y+15 w480 cBlue", "Cursor Colors:")
    settingsGui.AddText("xm+10 y+5 w480", "• GREEN: You have plenty of time to catch your train")
    settingsGui.AddText("xm+10 y+5 w480", "• YELLOW: You should leave soon to catch your train")
    settingsGui.AddText("xm+10 y+5 w480", "• RED: You won't make it to this train, wait for the next one")
    
    settingsGui.AddText("xm+10 y+15 w480 cBlue", "Additional Features:")
    settingsGui.AddText("xm+10 y+5 w480", "• Right-click the tray icon for quick options")
    settingsGui.AddText("xm+10 y+5 w480", "• The script only affects cursors between your set start and end times")
    settingsGui.AddText("xm+10 y+5 w480", "• Use 'Restore Default Cursors' to reset cursor appearance at any time")
    settingsGui.AddText("xm+10 y+15 w480", "Run this script automatically at startup for the best experience.")
    settingsGui.AddText("xm+10 y+15 w480", "Inspired by (and uses resources from) https://github.com/ivan-the-terrible/bloodsugar-cursor")
    
    ; Clear tab selection to add controls outside tabs
    tabs.UseTab(0)
    
    ; Save and cancel buttons - positioned at the bottom of the window outside any tab
    saveButton := settingsGui.AddButton("Default w100 x180", "Save Settings")
    cancelButton := settingsGui.AddButton("w100 x+20", "Cancel")
    
    ; Set event handlers for buttons
    saveButton.OnEvent("Click", (*) => SaveSettingsFunction())
    cancelButton.OnEvent("Click", (*) => (TrayTip("DART Cursor", "Settings not saved", 3), settingsGui.Destroy()))    ; Save settings function - separated from event handler
  
    SaveSettingsFunction() {
        ; Access global variables that will be modified
        global stationNames, currentStation, preferredDirection, minCatchableTime
        global yellowThreshold, redDisplayTime, checkFrequency, startTime, endTime, isActive
        global workModeEnabled, isWorkMode, homeStation, workStation, homeDirection, workDirection
        
        ; Validate inputs
        validationError := ""
          ; Get values from GUI
        selectedStationIndex := stationDropdown.Value
        if (!selectedStationIndex)
            validationError .= "• Please select a station.`n"
        
        ; Check if selected item is a section header
        selectedStationName := stationNames[selectedStationIndex]
        if (SubStr(selectedStationName, 1, 3) = "---")
            validationError .= "• Please select a station, not a section header.`n"
            
        ; Get direction value
        dirSelection := directionDropdown.Value
        newDirection := ""
        if (dirSelection = 2)
            newDirection := "Northbound"
        else if (dirSelection = 3)
            newDirection := "Southbound"
        ; else empty string for no filter
        
        ; Get time thresholds
        newMinCatchable := Integer(minCatchableEdit.Value)
        if (newMinCatchable < 0 || newMinCatchable > 30)
            validationError .= "• Minimum catchable time must be between 0 and 30 minutes.`n"
            
        newYellowThreshold := Integer(yellowEdit.Value)
        if (newYellowThreshold <= newMinCatchable || newYellowThreshold > 30)
            validationError .= "• Yellow threshold must be greater than minimum catchable time and no more than 30 minutes.`n"
              newRedDisplay := Integer(redDisplayEdit.Value)
        if (newRedDisplay < 10 || newRedDisplay > 300)
            validationError .= "• Red display time must be between 10 and 300 seconds.`n"
            
        newCheckFrequency := Integer(checkFrequencyEdit.Value)
        if (newCheckFrequency < 10 || newCheckFrequency > 600)
            validationError .= "• API check frequency must be between 10 and 600 seconds.`n"
            
        ; Validate time formats
        startHour := Integer(startHourEdit.Value)
        startMin := Integer(startMinEdit.Value)
        if (startHour < 0 || startHour > 23 || startMin < 0 || startMin > 59)
            validationError .= "• Start time must be in valid 24-hour format (00:00 - 23:59).`n"
        
        endHour := Integer(endHourEdit.Value)
        endMin := Integer(endMinEdit.Value)
        if (endHour < 0 || endHour > 23 || endMin < 0 || endMin > 59)
            validationError .= "• End time must be in valid 24-hour format (00:00 - 23:59).`n"
            
        ; Show errors if any
        if (validationError) {
            MsgBox("Please fix the following errors:`n`n" . validationError, "Validation Error", "Icon!")
            return
        }
          ; All validations passed, save settings
        ; Station (we already have selectedStationName from validation)
        if (!stations.Has(selectedStationName)) {
            MsgBox("Invalid station selection: " . selectedStationName, "Error", "Icon!")
            return
        }
        currentStation := stations[selectedStationName]
        
        ; Direction
        preferredDirection := newDirection        ; Time thresholds
        minCatchableTime := newMinCatchable
        yellowThreshold := newYellowThreshold
        redDisplayTime := newRedDisplay * 1000
        checkFrequency := newCheckFrequency * 1000        ; Schedule times
        startTime := Format("{:02d}{:02d}", startHour, startMin)
        endTime := Format("{:02d}{:02d}", endHour, endMin)
        
        ; Work/Home mode settings
        workModeEnabled := workModeCheck.Value
        
        ; Get selected stations and directions (only if Work/Home mode is enabled)
        if (workModeEnabled) {
            ; Get home station - validate dropdown selection and exclude section headers
            homeStationIndex := homeStationDropdown.Value
            if (homeStationIndex && homeStationIndex <= stationNames.Length) {
                homeStationName := stationNames[homeStationIndex]
                if (SubStr(homeStationName, 1, 3) != "---" && stations.Has(homeStationName)) {
                    homeStation := stations[homeStationName]
                }
            }
            
            ; Get work station - validate dropdown selection and exclude section headers
            workStationIndex := workStationDropdown.Value
            if (workStationIndex && workStationIndex <= stationNames.Length) {
                workStationName := stationNames[workStationIndex]
                if (SubStr(workStationName, 1, 3) != "---" && stations.Has(workStationName)) {
                    workStation := stations[workStationName]
                }
            }
            
            ; Get directions - convert dropdown indices to direction strings
            homeDirection := (homeDirectionDropdown.Value = 2) ? "Southbound" : "Northbound"
            workDirection := (workDirectionDropdown.Value = 2) ? "Southbound" : "Northbound"
              ; Apply current mode settings to active monitoring variables
            UpdateStationForMode()
        }
          ; Save to config file
        SaveConfig()
        
        ; Rebuild tray menu to reflect Work/Home mode changes (needed if enabling/disabling)
        InitializeTrayMenu()
        
        ; Remember the old active hours to check if they changed
        prevStartTime := startTime
        prevEndTime := endTime
          ; Force update if active
        if (isActive) {
            ; Restart the timer with the new frequency
            SetTimer(CheckNow, 0)  ; Stop current timer
            CheckNow()             ; Check immediately
            SetTimer(CheckNow, checkFrequency)  ; Restart with new frequency
        } else {
            ; We need to ensure cursors are restored if inactive
            CheckStartTime()  ; Check if we need to start monitoring now
            EnsureCursorsRestored()  ; Make sure cursors are restored if we're inactive
        }
            
        ; Show confirmation and close
        TrayTip("DART Cursor", "Settings saved successfully", 1)
        settingsGui.Destroy()
    }
      ; Show the GUI and activate it
    settingsGui.Show("AutoSize Center")
    WinActivate("DART Cursor Settings")
}

; Function to toggle between Work and Home modes
ToggleWorkHomeMode() {
    global isWorkMode, currentStation, homeStation, workStation, isActive, workModeEnabled
    global preferredDirection, homeDirection, workDirection
    
    if (!workModeEnabled) {
        TrayTip("DART Cursor", "Work/Home mode is not enabled. Enable it in Settings.", 3)
        return
    }
    
    ; Toggle the mode
    isWorkMode := !isWorkMode
    
    ; Update current station and direction based on mode
    UpdateStationForMode()
    
    ; Show notification
    modeText := isWorkMode ? "WORK" : "HOME"
    TrayTip("DART Cursor", "Switched to " . modeText . " mode - Station: " . currentStation . " (" . preferredDirection . ")", 2)
    LogMessage("Switched to " . modeText . " mode - Station: " . currentStation . " (" . preferredDirection . ")")    ; Save the current mode
    SaveConfig()
    
    ; Update tray menu toggle text to show updated toggle option
    UpdateWorkHomeModeToggle()
    
    ; Restart monitoring with new station if active
    if (isActive) {
        SetTimer(CheckNow, 0)  ; Stop current timer
        CheckNow()             ; Check immediately with new station
        SetTimer(CheckNow, checkFrequency)  ; Restart timer
    }
}

; Function to update current station and preferred direction based on Work/Home mode
UpdateStationForMode() {
    global workModeEnabled, isWorkMode, currentStation, homeStation, workStation
    global preferredDirection, homeDirection, workDirection
    
    if (workModeEnabled) {
        if (isWorkMode) {
            currentStation := workStation
            preferredDirection := workDirection
        } else {
            currentStation := homeStation
            preferredDirection := homeDirection
        }
    }
}

; Function to check if configuration is complete
IsConfigurationComplete() {
    global preferredDirection, currentStation, startTime, endTime
    
    ; Check for missing or empty values
    if (preferredDirection = "" || currentStation = "" || startTime = "" || endTime = "")
        return false
        
    ; Check for valid station
    stationFound := false
    for stationName, apiName in stations {
        if (apiName = currentStation) {
            stationFound := true
            break
        }
    }
    
    if (!stationFound)
        return false
        
    ; Check for valid time formats
    if (StrLen(startTime) != 4 || StrLen(endTime) != 4)
        return false
        
    ; All checks passed
    return true
}

; Function to ensure cursors are restored if monitoring is inactive
EnsureCursorsRestored() {
    global isActive
    
    if (!isActive) {
        LogMessage("Safety check: Ensuring cursors are restored when inactive")
        RestoreSystemCursors()
    }
}

; Cleanup on exit
ExitHandler(ExitReason, ExitCode) {
    LogMessage("Script exiting. Reason: " . ExitReason)
    
    ; Restore original cursors
    RestoreSystemCursors()
    
    ; Show notification that the script is closing, except for "Reload" exit reason
    if (ExitReason != "Reload")
        TrayTip("DART Cursor", "Script exiting - default cursors restored", 3)
    
    return 0
}

; ===== Main script execution =====
; Set up exit handler
OnExit(ExitHandler.Bind())

; Function to check for script suspension and handle cursor restoration
CheckSuspensionStatus(*) {
    static wasLastSuspended := false
    
    ; Check if the suspension state has changed
    if (A_IsSuspended != wasLastSuspended) {
        wasLastSuspended := A_IsSuspended
        LogMessage("Script suspension state changed: " . (A_IsSuspended ? "Suspended" : "Resumed"))
        
        ; Always restore cursors when suspended
        if (A_IsSuspended) {
            RestoreSystemCursors()
            LogMessage("Cursors restored due to script suspension")
        } else {
            ; When resuming, check if we need to reapply cursor colors
            if (isActive) {
                CheckNow()
            }
        }
    }
}

; Set up a timer to check for suspension status periodically
SetTimer(CheckSuspensionStatus, 60000)  ; 

; Initialize the script when it starts
InitializeScript()

; Function to handle tray menu, also giving the ability to toggle the Work/Home mode from the tray menu
InitializeTrayMenu() {
    global workModeEnabled, isWorkMode
    
    ; Clear any existing custom menu items first to avoid duplication
    try A_TrayMenu.Delete("Check Train Times Now")
    try A_TrayMenu.Delete("Switch to HOME mode")
    try A_TrayMenu.Delete("Switch to WORK mode")
    try A_TrayMenu.Delete("Open DART Cursor Settings")
    try A_TrayMenu.Delete("Restore Default Cursors")
    
    ; Add our custom items
    A_TrayMenu.Insert("1&", "Check Train Times Now", CheckNow)
    A_TrayMenu.Insert("2&", "") ; Separator
    
    ; Add Work/Home mode toggle if enabled
    if (workModeEnabled) {
        modeText := "Switch to " . (isWorkMode ? "HOME" : "WORK") . " mode"
        A_TrayMenu.Insert("3&", modeText, (*) => ToggleWorkHomeMode())
        A_TrayMenu.Insert("4&", "") ; Separator
    }
    
    ; Add remaining items at the end
    A_TrayMenu.Insert("", "Open DART Cursor Settings", OpenSettingsGUI)
    A_TrayMenu.Insert("", "Restore Default Cursors", RestoreDefaultCursors)
    A_TrayMenu.Insert("", "") ; Separator
    
    A_TrayMenu.Default := "Open DART Cursor Settings"
}


UpdateWorkHomeModeToggle() {
    global workModeEnabled, isWorkMode
    
    if (workModeEnabled) {
        ; Find the current toggle item and update its text
        oldText := "Switch to " . (isWorkMode ? "WORK" : "HOME") . " mode"
        newText := "Switch to " . (isWorkMode ? "HOME" : "WORK") . " mode"
        
        try {
            A_TrayMenu.Rename(oldText, newText)
        } catch {
            ; If rename fails, the menu might need to be rebuilt (settings changed)
            InitializeTrayMenu()
        }
    }
}
