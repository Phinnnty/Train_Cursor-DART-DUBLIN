# PowerShell script to convert cursors using GIMP Python interface
# This approach is more reliable than Script-Fu for batch processing

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

# Create a Python script for GIMP
$PythonScript = @'
import sys
import os
from gimpfu import *

def convert_cursor_hue(input_file, output_file):
    # Load the image
    image = pdb.gimp_file_load(input_file, input_file)
    
    # Get all layers
    layers = image.layers
    
    # Apply hue-saturation to each layer
    for layer in layers:
        # Apply -23 hue shift to yellows (channel 2)
        pdb.gimp_drawable_hue_saturation(layer, 2, -23, 0, 0, 0)
    
    # Save the file
    pdb.gimp_file_save(image, layers[0], output_file, output_file)
    
    # Clean up
    pdb.gimp_image_delete(image)

# Process the file passed as argument
if len(sys.argv) >= 3:
    input_path = sys.argv[1]
    output_path = sys.argv[2]
    convert_cursor_hue(input_path, output_path)

pdb.gimp_quit(1)
'@

$PythonScriptFile = Join-Path $ScriptDir "convert_single_cursor.py"
$PythonScript | Out-File -FilePath $PythonScriptFile -Encoding UTF8

# Process each cursor file individually
$ProcessedCount = 0
foreach ($CursorFile in $CursorFiles) {
    $InputPath = $CursorFile.FullName
    $OutputPath = Join-Path $OutputDir $CursorFile.Name
    
    Write-Host "Processing: $($CursorFile.Name)"
    
    # Create individual command for this file
    $Command = @"
import sys
sys.path.append('$($ScriptDir -replace '\\', '/')')
from gimpfu import *

# Load the image
image = pdb.gimp_file_load('$($InputPath -replace '\\', '/')', '$($InputPath -replace '\\', '/')')

# Get all layers
layers = image.layers

# Apply hue-saturation to each layer  
for layer in layers:
    pdb.gimp_drawable_hue_saturation(layer, 2, -23, 0, 0, 0)

# Save the file
pdb.gimp_file_save(image, layers[0], '$($OutputPath -replace '\\', '/')', '$($OutputPath -replace '\\', '/')')

# Clean up
pdb.gimp_image_delete(image)
pdb.gimp_quit(1)
"@
    
    # Run GIMP with this specific command
    try {
        & $GimpExe --no-interface --batch-interpreter "python-fu-eval" --batch $Command --quit
        $ProcessedCount++
        Write-Host "  ✓ Completed: $($CursorFile.Name)"
    } catch {
        Write-Host "  ✗ Failed: $($CursorFile.Name) - $($_.Exception.Message)"
    }
}

Write-Host ""
Write-Host "Conversion completed! Processed $ProcessedCount out of $($CursorFiles.Count) files."
Write-Host "Orange cursors are in: $OutputDir"

# Clean up
Remove-Item $PythonScriptFile -ErrorAction SilentlyContinue
