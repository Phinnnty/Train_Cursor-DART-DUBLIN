# PowerShell script to automate GIMP cursor conversion
# This script creates a GIMP batch command file and runs it

param(
    [string]$InputDir = "elevated_cursors",
    [string]$OutputDir = "orange_cursors"
)

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$InputPath = Join-Path $ScriptDir $InputDir
$OutputPath = Join-Path $ScriptDir $OutputDir

# Create output directory if it doesn't exist
if (!(Test-Path $OutputPath)) {
    New-Item -ItemType Directory -Path $OutputPath -Force
    Write-Host "Created directory: $OutputPath"
}

# Create GIMP batch script with proper Scheme syntax
$InputPathFixed = $InputPath -replace '\\', '/'
$OutputPathFixed = $OutputPath -replace '\\', '/'

$GimpScript = @"
(let* ((input-dir "$InputPathFixed")
       (output-dir "$OutputPathFixed")
       (pattern (string-append input-dir "/*.cur"))
       (filelist (cadr (file-glob pattern 1))))
  (while (not (null? filelist))
    (let* ((filename (car filelist))
           (image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
           (basename (car (reverse (strbreakup filename "/"))))
           (output-file (string-append output-dir "/" basename)))
      
      ; Get all layers and apply hue shift to each
      (let* ((layers (cadr (gimp-image-get-layers image)))
             (num-layers (car layers))
             (layer-array (cadr layers)))
        (let ((i 0))
          (while (< i num-layers)
            (let ((layer (aref layer-array i)))
              ; Apply -23 hue shift to yellows channel
              (gimp-drawable-hue-saturation layer 2 -23 0 0 0))
            (set! i (+ i 1)))))
      
      ; Export the file (try different export methods for GIMP 3)
      (gimp-file-save RUN-NONINTERACTIVE image output-file)
      (gimp-image-delete image))
    (set! filelist (cdr filelist)))
  (gimp-quit 0))
"@

$GimpScriptFile = Join-Path $ScriptDir "temp_batch_script.scm"
$GimpScript | Out-File -FilePath $GimpScriptFile -Encoding UTF8

Write-Host "Created GIMP script: $GimpScriptFile"
Write-Host "Processing cursor files from $InputPath to $OutputPath..."

# Try to find GIMP executable
$GimpPaths = @(
    "C:\Users\Fintw\AppData\Local\Programs\GIMP 3\bin\gimp-3.exe",
    "C:\Users\Fintw\AppData\Local\Programs\GIMP 3\bin\gimp-console-3.exe",
    "C:\Users\Fintw\OneDrive\Desktop\GIMP 2\bin\gimp-2.10.exe",
    "C:\Users\Fintw\OneDrive\Desktop\GIMP 2\bin\gimp-console-2.10.exe",
    "C:\Users\Fintw\OneDrive\Desktop\gimp-2.10.exe",
    "C:\Users\Fintw\OneDrive\Desktop\GIMP\bin\gimp-2.10.exe",
    "C:\Users\Fintw\OneDrive\Desktop\GIMP\bin\gimp-console-2.10.exe",
    "C:\Program Files\GIMP 2\bin\gimp-2.10.exe",
    "C:\Program Files (x86)\GIMP 2\bin\gimp-2.10.exe",
    "C:\Program Files\GIMP 2\bin\gimp-console-2.10.exe",
    "C:\Program Files (x86)\GIMP 2\bin\gimp-console-2.10.exe"
)

$GimpExe = $null
foreach ($path in $GimpPaths) {
    if (Test-Path $path) {
        $GimpExe = $path
        break
    }
}

if ($GimpExe) {
    Write-Host "Found GIMP at: $GimpExe"
    Write-Host "Running batch conversion..."
    
    # Run GIMP with the batch script - specify Script-Fu interpreter for GIMP 3
    if ($GimpExe -like "*gimp-3*") {
        & $GimpExe --no-interface --batch-interpreter "plug-in-script-fu-eval" --batch "($GimpScript)" --batch "(gimp-quit 0)" --quit
    } else {
        # GIMP 2.x syntax
        & $GimpExe --no-interface --batch "($GimpScript)" --batch "(gimp-quit 0)"
    }
    
    Write-Host "Batch conversion completed!"
} else {
    Write-Host "GIMP not found. Please install GIMP or update the paths in this script."
    Write-Host "You can manually run the generated script file: $GimpScriptFile"
    Write-Host "In GIMP: Filters > Script-Fu > Console, then load and run the script."
}

# Clean up temporary script file
Remove-Item $GimpScriptFile -ErrorAction SilentlyContinue
