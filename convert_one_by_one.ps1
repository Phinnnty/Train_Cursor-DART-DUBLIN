# Simple one-by-one cursor conversion script
# This processes each cursor file individually to avoid GIMP batch issues

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$InputDir = Join-Path $ScriptDir "elevated_cursors"
$OutputDir = Join-Path $ScriptDir "orange_cursors"
$GimpExe = "C:\Users\Fintw\AppData\Local\Programs\GIMP 3\bin\gimp-3.exe"

# Create output directory if it doesn't exist
if (!(Test-Path $OutputDir)) {
    New-Item -ItemType Directory -Path $OutputDir -Force
    Write-Host "Created directory: $OutputDir"
}

# Get all .cur files
$CursorFiles = Get-ChildItem -Path $InputDir -Filter "*.cur"

Write-Host "Found $($CursorFiles.Count) cursor files to process..."

foreach ($File in $CursorFiles) {
    $InputFile = $File.FullName -replace '\\', '/'
    $OutputFile = (Join-Path $OutputDir $File.Name) -replace '\\', '/'
    
    Write-Host "Processing: $($File.Name)"
    
    # Create a simple script for this specific file
    $SingleFileScript = @"
(let* ((image (car (gimp-file-load RUN-NONINTERACTIVE "$InputFile" "$InputFile")))
       (layers (cadr (gimp-image-get-layers image)))
       (num-layers (car layers))
       (layer-array (cadr layers)))
  (let ((i 0))
    (while (< i num-layers)
      (let ((layer (vector-ref layer-array i)))
        (gimp-drawable-hue-saturation layer 2 -23 0 0 0))
      (set! i (+ i 1))))
  (gimp-file-save RUN-NONINTERACTIVE image (vector-ref layer-array 0) "$OutputFile" "$OutputFile")
  (gimp-image-delete image)
  (gimp-quit 0))
"@

    # Write script to temp file
    $TempScript = Join-Path $ScriptDir "temp_single.scm"
    $SingleFileScript | Out-File -FilePath $TempScript -Encoding UTF8
    
    # Run GIMP for this single file
    $TempScriptFixed = $TempScript -replace '\\', '/'
    try {
        & $GimpExe --no-interface --batch-interpreter "plug-in-script-fu-eval" --batch "($SingleFileScript)" --quit 2>$null
        Write-Host "  ✓ Completed: $($File.Name)"
    }
    catch {
        Write-Host "  ✗ Error processing: $($File.Name)"
    }
    
    # Clean up temp file
    if (Test-Path $TempScript) {
        Remove-Item $TempScript -Force
    }
    
    # Small delay to prevent issues
    Start-Sleep -Milliseconds 500
}

Write-Host "`nConversion completed! Check the orange_cursors folder."
Write-Host "Processed $($CursorFiles.Count) files."
