#!/usr/bin/env python3
"""
GIMP Python script to batch convert yellow cursors to orange
Run this from GIMP's Python console or as a GIMP plugin
"""

import os
from gimpfu import *

def batch_cursor_to_orange(input_dir, output_dir):
    """
    Convert all .cur files in input_dir from yellow to orange
    Apply -23 hue shift to all layers in each file
    """
    
    # Ensure output directory exists
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)
    
    # Get all .cur files from input directory
    cur_files = [f for f in os.listdir(input_dir) if f.lower().endswith('.cur')]
    
    for filename in cur_files:
        input_path = os.path.join(input_dir, filename)
        output_path = os.path.join(output_dir, filename)
        
        try:
            # Load the cursor file
            image = pdb.gimp_file_load(input_path, input_path)
            
            # Get all layers
            layers = image.layers
            
            # Process each layer
            for layer in layers:
                # Apply hue-saturation: channel=2 (YELLOWS), hue=-23, saturation=0, lightness=0
                pdb.gimp_drawable_hue_saturation(layer, 2, -23, 0, 0, 0)
            
            # Save the modified cursor file
            # Note: You might need to adjust this depending on GIMP's cursor export capabilities
            pdb.gimp_file_save(image, layers[0], output_path, output_path)
            
            # Clean up
            pdb.gimp_image_delete(image)
            
            print(f"Processed: {filename}")
            
        except Exception as e:
            print(f"Error processing {filename}: {str(e)}")

# Register the plugin
register(
    "python_fu_batch_cursor_orange",
    "Batch Convert Cursors to Orange",
    "Converts all .cur files from yellow to orange with -23 hue shift",
    "User",
    "User",
    "2025",
    "Batch Cursor to Orange...",
    "",
    [
        (PF_DIRNAME, "input_dir", "Input Directory (elevated_cursors)", ""),
        (PF_DIRNAME, "output_dir", "Output Directory (orange_cursors)", "")
    ],
    [],
    batch_cursor_to_orange,
    menu="<Image>/Filters/Batch"
)

main()
