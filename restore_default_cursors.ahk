#Requires AutoHotkey v2.0
#SingleInstance Force

; Manual cursor restore script
; Run this to restore default Windows cursors

TrayTip("Cursor Restore", "Restoring default Windows cursors...", 2)

; Restore default system cursors
RestoreSystemCursors()

; Function to restore original Windows cursors
RestoreSystemCursors() {
    ; Force Windows to reload default cursors
    DllCall("SystemParametersInfo", "UInt", 0x57, "UInt", 0, "Ptr", 0, "UInt", 0)  ; SPI_SETCURSORS
    
    ; Additional method to ensure cursor refresh
    DllCall("User32.dll\SystemParametersInfo", "UInt", 0x0057, "UInt", 0, "Ptr", 0, "UInt", 0x01)
    
    ; Force cursor update
    DllCall("User32.dll\SetCursor", "Ptr", DllCall("User32.dll\LoadCursor", "Ptr", 0, "Ptr", 32512, "Ptr"))
    
    TrayTip("Success", "Default cursors restored!", 2)
}

; Auto-exit after 3 seconds
SetTimer(() => ExitApp(), 3000)
