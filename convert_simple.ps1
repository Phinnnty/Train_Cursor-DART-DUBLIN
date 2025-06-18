# Simple PowerShell script to convert cursors one by one
# Uses individual GIMP commands to avoid batch processing issues

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$InputDir = Join-Path $ScriptDir "elevated_cursors"
$OutputDir = Join-Path $ScriptDir "orange_cursors"
$GimpExe = "C:\Users\Fintw\AppData\Local\Programs\GIMP 3\bin\gimp-3.exe"

# Create output directory
if (!(Test-Path $OutputDir)) {
    New-Item -ItemType Directory -Path $OutputDir -Force
    Write-Host "Created directory: $OutputDir"
}

# Get all cursor files
$CursorFiles = Get-ChildItem $InputDir -Filter "*.cur"
Write-Host "Found $($CursorFiles.Count) cursor files to process"

# Process each cursor file individually
$ProcessedCount = 0
foreach ($CursorFile in $CursorFiles) {
    $InputPath = ($CursorFile.FullName -replace '\\', '/')
    $OutputPath = ((Join-Path $OutputDir $CursorFile.Name) -replace '\\', '/')
    
    Write-Host "Processing: $($CursorFile.Name)"
    
    # Create a simple Script-Fu command with proper quoting
    $ScriptFuCommand = "(let* ((image (car (gimp-file-load RUN-NONINTERACTIVE `"$InputPath`" `"$InputPath`"))) (layers (cadr (gimp-image-get-layers image))) (layer-array (cadr layers))) (let ((i 0)) (while (< i (car layers)) (gimp-drawable-hue-saturation (vector-ref layer-array i) 2 -23 0 0 0) (set! i (+ i 1)))) (gimp-file-save RUN-NONINTERACTIVE image (vector-ref layer-array 0) `"$OutputPath`" `"$OutputPath`") (gimp-image-delete image) (gimp-quit 0))"
    
    # Run GIMP with this command
    & $GimpExe --no-interface --batch-interpreter "plug-in-script-fu-eval" --batch $ScriptFuCommand --quit
    
    $ProcessedCount++
    Write-Host "  Completed: $($CursorFile.Name)"
}

Write-Host ""
Write-Host "Conversion completed! Processed $ProcessedCount out of $($CursorFiles.Count) files."
Write-Host "Orange cursors are in: $OutputDir"
