# Direct PowerShell approach - create the command file and run it differently

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$OutputDir = Join-Path $ScriptDir "orange_cursors"
$InputDir = Join-Path $ScriptDir "elevated_cursors"

# Create output directory if it doesn't exist
if (!(Test-Path $OutputDir)) {
    New-Item -ItemType Directory -Path $OutputDir -Force
    Write-Host "Created directory: $OutputDir"
}

# GIMP executable
$GimpExe = "C:\Users\Fintw\AppData\Local\Programs\GIMP 3\bin\gimp-3.exe"

if (Test-Path $GimpExe) {
    Write-Host "Found GIMP at: $GimpExe"
    Write-Host "Processing cursor files..."
    
    # Get all .cur files from elevated_cursors
    $CursorFiles = Get-ChildItem "$InputDir\*.cur"
    
    foreach ($file in $CursorFiles) {
        $InputFile = $file.FullName -replace '\\', '/'
        $OutputFile = (Join-Path $OutputDir $file.Name) -replace '\\', '/'
        
        Write-Host "Processing: $($file.Name)"
        
        # Create a temporary script for each file
        $TempScript = @"
(let* ((image (car (gimp-file-load RUN-NONINTERACTIVE "$InputFile" "$InputFile")))
       (layers (cadr (gimp-image-get-layers image)))
       (num-layers (car layers))
       (layer-array (cadr layers)))
  (let ((i 0))
    (while (< i num-layers)
      (let ((layer (aref layer-array i)))
        (gimp-drawable-hue-saturation layer 2 -23 0 0 0))
      (set! i (+ i 1))))
  (gimp-file-save RUN-NONINTERACTIVE image "$OutputFile")
  (gimp-image-delete image)
  (gimp-quit 0))
"@
        
        # Save temp script and run it
        $TempScriptFile = Join-Path $ScriptDir "temp_single.scm"
        $TempScript | Out-File -FilePath $TempScriptFile -Encoding UTF8
        
        # Run GIMP with this single file
        & $GimpExe --no-interface --batch-interpreter "plug-in-script-fu-eval" --batch $TempScript --quit
        
        # Clean up temp file
        Remove-Item $TempScriptFile -ErrorAction SilentlyContinue
    }
    
    Write-Host "All cursor files processed!"
    Write-Host "Check the orange_cursors folder for your converted files."
} else {
    Write-Host "GIMP not found at: $GimpExe"
}
