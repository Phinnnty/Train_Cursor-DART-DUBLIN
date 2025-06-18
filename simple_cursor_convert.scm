;; Simple Script-Fu to convert yellow cursors to orange
;; This script will be called by PowerShell

(define (convert-cursors-to-orange)
  (let* ((input-dir "C:/GitHub/Train_Cursor-DART-DUBLIN/elevated_cursors")
         (output-dir "C:/GitHub/Train_Cursor-DART-DUBLIN/orange_cursors")
         (file-pattern (string-append input-dir "/*.cur"))
         (file-list (car (file-glob file-pattern 1))))
    
    ; Create output directory if it doesn't exist
    (if (not (file-exists? output-dir))
        (file-mkdir output-dir))
    
    ; Process each file
    (while (pair? file-list)
      (let* ((input-file (car file-list))
             (filename (car (last (strbreakup input-file "/"))))
             (output-file (string-append output-dir "/" filename)))
        
        ; Load the image
        (let ((image (car (gimp-file-load RUN-NONINTERACTIVE input-file input-file))))
          
          ; Get all layers
          (let* ((layer-info (gimp-image-get-layers image))
                 (num-layers (car layer-info))
                 (layer-array (cadr layer-info)))
            
            ; Apply hue shift to each layer
            (let ((i 0))
              (while (< i num-layers)
                (let ((layer (vector-ref layer-array i)))
                  ; Apply -23 hue shift to yellows (channel 2)
                  (gimp-drawable-hue-saturation layer 2 -23 0 0 0))
                (set! i (+ i 1))))
            
            ; Save the modified image
            (gimp-file-save RUN-NONINTERACTIVE image (vector-ref layer-array 0) output-file output-file)
            
            ; Clean up
            (gimp-image-delete image)))
        
        ; Move to next file
        (set! file-list (cdr file-list))))
    
    ; Done
    (gimp-message "Cursor conversion completed!")
    ))

; Run the conversion
(convert-cursors-to-orange)
(gimp-quit 0)
