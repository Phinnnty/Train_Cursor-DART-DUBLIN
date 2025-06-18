# Simple PowerShell script to run the cursor conversion
# Uses a separate .scm file for cleaner execution

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$ScriptFile = Join-Path $ScriptDir "simple_cursor_convert.scm"
$OutputDir = Join-Path $ScriptDir "orange_cursors"

# Create output directory if it doesn't exist
if (!(Test-Path $OutputDir)) {
    New-Item -ItemType Directory -Path $OutputDir -Force
    Write-Host "Created directory: $OutputDir"
}

# Find GIMP executable
$GimpExe = "C:\Users\Fintw\AppData\Local\Programs\GIMP 3\bin\gimp-3.exe"

if (Test-Path $GimpExe) {
    Write-Host "Found GIMP at: $GimpExe"
    Write-Host "Running cursor conversion script..."
    
    # Run GIMP with the script file
    $ScriptFileFixed = $ScriptFile -replace '\\', '/'
    & $GimpExe --no-interface --batch-interpreter "plug-in-script-fu-eval" --batch "(load `"$ScriptFileFixed`")" --quit
    
    Write-Host "Batch conversion completed!"
    Write-Host "Check the orange_cursors folder for your converted files."
} else {
    Write-Host "GIMP not found at: $GimpExe"
    Write-Host "Please update the GimpExe path in this script."
}
