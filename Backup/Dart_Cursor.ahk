#Requires AutoHotkey v2.0
#SingleInstance Force

; ============================
; Dart Cursor - Train Arrival Notification via Cursor Color
; ============================

; ToDo: 
; Add in functions to set the start and finish times for the script

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
global yellowThreshold := 3       ; Minutes above catchable time for yellow warning (10-7 minutes)
global redDisplayTime := 60000    ; How long to show red cursor before moving to next train (ms)

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

; Function to initialize the script
InitializeScript() {
    global logFile
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
    ; Load the configuration if it exists
    if FileExist(configFile) {
        global preferredDirection := IniRead(configFile, "Settings", "Direction", "")
        global currentStation := IniRead(configFile, "Settings", "Station", "Lansdowne Road")
        LogMessage("Loaded configuration. Station: " . currentStation . ", Direction: " . (preferredDirection ? preferredDirection : "Not set"))
        
        ; Load time thresholds
        global minCatchableTime := Integer(IniRead(configFile, "Settings", "MinCatchableTime", "5"))
        global yellowThreshold := Integer(IniRead(configFile, "Settings", "YellowThreshold", "5"))
        global redDisplayTime := Integer(IniRead(configFile, "Settings", "RedDisplayTime", "60")) * 1000
        
        LogMessage("Loaded time thresholds - Min catchable: " . minCatchableTime . ", Yellow threshold: " . yellowThreshold . ", Red display time: " . (redDisplayTime/1000) . "s")
        
        if (preferredDirection = "")
            PromptForDirection()
    } else {
        LogMessage("No configuration file found")
        PromptForDirection()
    }
    ; Set up tray menu
    A_TrayMenu.Add("Change Station", ChangeStation)
    A_TrayMenu.Add("Change Direction", ChangeDirection)
    A_TrayMenu.Add("Check Now", CheckNow)
    A_TrayMenu.Add() ; Separator
    A_TrayMenu.Add("Set Minimum Catchable Time", SetCatchableTime)
    A_TrayMenu.Add("Set Yellow Threshold", SetYellowThreshold)
    A_TrayMenu.Add("Set Red Display Time", SetRedDisplayTime)
    A_TrayMenu.Add()  ; Separator
    A_TrayMenu.Default := "Change Station"
    LogMessage("Tray menu initialized")
    
    ; Check if the task already exists for tomorrow
    taskName := "DARTCursorMonitor"
    tomorrow := FormatTime(DateAdd(A_Now, 1, "days"), "yyyyMMdd")
    
    ; Try to query if task exists
    verifyCmd := 'schtasks /query /tn "' . taskName . '"'
    RunWait(verifyCmd, , "Hide", &verifyCode)
    
    ; If task doesn't exist, create it
    if (verifyCode != 0) {
        LogMessage("No scheduled task found for tomorrow, creating one")
        ScheduleNextRun()
    } else {
        LogMessage("Scheduled task already exists")
    }
    
    ; Continue with time-based activation
    CheckStartTime()
}

; Function to prompt the user for their preferred direction
PromptForDirection() {
    result := MsgBox("Which direction do you travel from this Station?"
        "`n`nYes = Northbound"
        "`nNo = Southbound"
        "`nCancel = Don't set now",
        "DART Direction Setup", "YesNoCancel Icon?")
    
    global preferredDirection
    
    if (result = "Yes") {
        preferredDirection := "Northbound"
        SaveConfig()
        LogMessage("Direction set to Northbound")
    } else if (result = "No") {
        preferredDirection := "Southbound"
        SaveConfig()
        LogMessage("Direction set to Southbound")
    } else {
        ; Default to showing all trains if user cancels
        preferredDirection := ""
        LogMessage("No direction selected, will show all trains")
    }
    
    ; Force an immediate check after direction change
    global isActive
    if (isActive)
        CheckNow()
}

; Function to save configuration to a file
SaveConfig() {
    global preferredDirection, currentStation, minCatchableTime, yellowThreshold, redDisplayTime
    IniWrite(preferredDirection, configFile, "Settings", "Direction")
    IniWrite(currentStation, configFile, "Settings", "Station")
    IniWrite(minCatchableTime, configFile, "Settings", "MinCatchableTime")
    IniWrite(yellowThreshold, configFile, "Settings", "YellowThreshold")
    IniWrite(redDisplayTime/1000, configFile, "Settings", "RedDisplayTime") ; Store in seconds
}

; Function to allow changing direction from tray menu
ChangeDirection(*) {
    PromptForDirection()
}

; Function to allow changing station from tray menu
ChangeStation(*) {
    global currentStation, stations
    
    ; Create a GUI with dropdown and autocomplete
    stationGui := Gui("+AlwaysOnTop", "Select DART Station")
    
    ; Add instruction text
    stationGui.AddText("w380", "Select your DART station (current: " . currentStation . "):")    ; Create an array of station names for the dropdown
    stationNames := []
    for stationName, apiName in stations {
        stationNames.Push(stationName)
    }    ; Sort the station names alphabetically (using array method instead of Sort function)
    stationNames := SortArray(stationNames)
    
    ; Add the ComboBox with dropdown list style
    stationCombo := stationGui.AddComboBox("w380")  ; Simple dropdown
      ; Add items to the ComboBox
    for i, stationName in stationNames {
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
    stationGui.AddButton("w100 x170 y+20 Default", "Select").OnEvent("Click", SelectStation)
    stationGui.AddButton("w100 x+10 y+-25", "Cancel").OnEvent("Click", (*) => stationGui.Destroy())
      ; Function to handle station selection
    SelectStation(*) {
        ; Get the selected text from the combo box
        selectedIndex := stationCombo.Value
        selectedStation := stationNames[selectedIndex]  ; Get station name by index
        
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
            stationGui.Destroy()
        } else {
            ; No exact match found, warn user
            MsgBox("'" . selectedStation . "' is not a recognized DART station. Please select a station from the dropdown list.", 
                   "Invalid Station", "Icon!")
        }
    }
    
    ; Show the GUI
    stationGui.Show()
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
    global yellowThreshold
    
    userInput := InputBox("Enter minutes above minimum catchable time for yellow warning (current: " . yellowThreshold . "):", 
                        "Yellow Warning Threshold", "w300 h120", yellowThreshold)
      if (userInput.Result = "OK") {
        newTime := Integer(userInput.Value)
        if (newTime >= 1 && newTime <= 30) {
            yellowThreshold := newTime
            LogMessage("Yellow threshold set to " . yellowThreshold . " minutes")
            ; Save to config
            IniWrite(yellowThreshold, configFile, "Settings", "YellowThreshold")
            
            ; Force an update
            if (isActive)
                CheckNow()
        } else {
            MsgBox("Please enter a number between 1 and 30", "Invalid Input", "Iconi")
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
    currentTime := FormatTime(, "HHmm")
    currentHour := SubStr(currentTime, 1, 2)
    currentDate := FormatTime(, "yyyyMMdd")
    
    ; If it's between 5:10 PM (1710 hours) and 5:45 PM (1745 hours), start checking trains
    if (currentTime >= 1710 && currentTime < 1745) {
        ; Only log and activate if we weren't already active
        if (!isActive) {
            global isActive := true
            LogMessage("Starting train checks at " . FormatTime(, "HH:mm:ss"))
            StartChecking()
        }
    } else if (currentTime >= 1745) {
        ; After 5:45 PM, deactivate if active and exit the script
        if (isActive) {
            global isActive := false
            LogMessage("Stopping train checks at " . FormatTime(, "HH:mm:ss") . " (after 5:45 PM)")
            SetTimer(CheckNow, 0)  ; Stop the timer
            RestoreSystemCursors()  ; Restore normal cursors
            
            ; Schedule the script to run tomorrow at 17:10 using Windows Task Scheduler
            ScheduleNextRun()
              ; Exit the script
            LogMessage("Exiting script - will restart tomorrow at 17:10")
            Sleep(500)  ; Give a moment for log to be written
            ExitApp
        } else {
            ; Script is already inactive but it's after end time - exit
            LogMessage("Outside active hours (after 5:45 PM) - exiting script")
            ; Ensure cursors are restored even if script wasn't active
            RestoreSystemCursors()
            ScheduleNextRun()            LogMessage("Exiting script - will restart tomorrow at 17:10") 
            Sleep(500)  ; Give a moment for log to be written
            ExitApp
        }
    } else {
        ; Before start time, wait and check again
        LogMessage("Waiting for start time (17:10) - current time: " . FormatTime(, "HH:mm:ss"))
        
        ; If it's getting close to start time, check more frequently
        if (currentTime >= 1700) {
            SetTimer(CheckStartTime, 30000)  ; Check every 30 seconds
        } else {
            SetTimer(CheckStartTime, 60000)  ; Check every minute
        }
    }
}

; Function to schedule the script to run every weekday at 17:11
ScheduleNextRun() {
    scriptPath := A_ScriptFullPath
    autohotkeyPath := "C:\Program Files\AutoHotkey\v2\AutoHotkey64.exe"  ; Adjust path if needed
    taskName := "DARTCursorMonitor"
    
    ; First check if the task already exists
    LogMessage("Checking if task already exists: " . taskName)
    
    checkCmd := "powershell.exe -Command " . Chr(34) . 
                "if (Get-ScheduledTask -TaskName '" . taskName . "' -ErrorAction SilentlyContinue) " .
                "{ Write-Output 'Task exists'; exit 0 } else { Write-Output 'Task does not exist'; exit 1 }" . 
                Chr(34)
                
    try {
        RunWait(checkCmd, , "Hide", &checkCode)
        
        ; If task exists (exit code 0), delete it first
        if (checkCode = 0) {
            LogMessage("Task already exists - will remove and recreate")
            
            deleteCmd := "powershell.exe -Command " . Chr(34) .
                        "Unregister-ScheduledTask -TaskName '" . taskName . "' -Confirm:`$false" .
                        Chr(34)
                        
            RunWait(deleteCmd, , "Hide", &deleteCode)
            LogMessage("Task deletion result: " . (deleteCode = 0 ? "Success" : "Failed with code " . deleteCode))
        } else {
            LogMessage("No existing task found with name: " . taskName)
        }
        
        ; Now create the task (whether it existed before or not)
        LogMessage("Creating scheduled task to run script on weekdays at 17:11")
        
        ; Build PowerShell command with simpler approach to avoid quoting issues
        ; Use single quotes for PowerShell strings that contain paths
        psCmd := "powershell.exe -Command " . Chr(34) . 
                "$action = New-ScheduledTaskAction -Execute '" . autohotkeyPath . "' -Argument '" . scriptPath . "'; " .
                "$trigger = New-ScheduledTaskTrigger -Weekly -DaysOfWeek Monday,Tuesday,Wednesday,Thursday,Friday -At '17:11'; " .
                "$settings = New-ScheduledTaskSettingsSet -AllowStartIfOnBatteries -DontStopIfGoingOnBatteries; " .
                "$task = New-ScheduledTask -Action $action -Trigger $trigger -Settings $settings; " .
                "Register-ScheduledTask -TaskName '" . taskName . "' -InputObject $task -User '" . A_UserName . "' -Force" .
                Chr(34)
        
        LogMessage("PowerShell command: " . SubStr(psCmd, 1, 100) . "...")
        
        ; Try to create the task
        RunWait(psCmd, , "Hide", &exitCode)
        
        if (exitCode = 0) {
            LogMessage("Scheduled task created successfully")
            MsgBox("DART Cursor script scheduled to run every weekday at 17:11", "Task Scheduled", "Info")
            return true
        } else {
            LogMessage("Failed to create scheduled task. Exit code: " . exitCode)
            
            ; Try with elevated permissions
            MsgBox("Failed to create scheduled task. The script will now try with elevated permissions.", "Elevation Required", "Info")
            
            RunWait("*RunAs " . psCmd, , "Hide", &elevatedExitCode)
            
            if (elevatedExitCode = 0) {
                LogMessage("Scheduled task created successfully with elevation")
                MsgBox("DART Cursor script scheduled to run every weekday at 17:11", "Task Scheduled", "Info")
                return true
            } else {
                LogMessage("Failed to create task even with elevation. Code: " . elevatedExitCode)
                MsgBox("Failed to create scheduled task. You may need to manually schedule this script.", "Task Creation Failed", "Icon!")
            }
        }
    } catch as e {
        LogMessage("Exception when working with scheduled task: " . e.Message)
        MsgBox("Error with scheduled task: " . e.Message, "Schedule Error", "Icon!")
    }
    
    return false
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
    } else if (minutesUntilArrival <= minCatchableTime + yellowThreshold) {
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
    nextTrainDue := trainData[currentTrainIndex].due      ; Determine cursor status based on arrival time according to specified rules:
    ; Green by default
    ; Yellow when train is arriving between minCatchableTime + yellowThreshold and minCatchableTime
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
    } else if (nextTrainDue <= minCatchableTime + yellowThreshold) {
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
    } else if (status = "yellow") {
        cursorsFolderPath := A_ScriptDir . "\elevated_cursors\"
        LogMessage("Setting YELLOW cursors (leave soon)")
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

; Cleanup on exit
ExitHandler(ExitReason, ExitCode) {
    LogMessage("Script exiting. Reason: " . ExitReason)
    
    ; Restore original cursors
    RestoreSystemCursors()
    
    return 0
}

; ===== Main script execution =====
; Set up exit handler
OnExit(ExitHandler.Bind())

; Initialize the script when it starts
InitializeScript()
