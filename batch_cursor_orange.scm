;; GIMP Script-Fu to batch convert yellow cursors to orange
;; This script processes all .cur files in elevated_cursors folder
;; and saves orange versions to orange_cursors folder

(define (batch-cursor-to-orange input-dir output-dir)
  (let* ((filelist (cadr (file-glob (string-append input-dir "/*.cur") 1))))
    (while (not (null? filelist))
      (let* ((filename (car filelist))
             (image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
             (basename (car (gimp-image-get-name image)))
             (output-file (string-append output-dir "/" basename)))
        
        ;; Get all layers in the image
        (let* ((layers (cadr (gimp-image-get-layers image))))
          ;; Process each layer
          (let ((layer-list (vector->list layers)))
            (while (not (null? layer-list))
              (let ((layer (car layer-list)))
                ;; Apply hue-saturation adjustment (-23 hue shift)
                (gimp-drawable-hue-saturation layer 2 -23 0 0 0)  ;; 2 = YELLOWS channel
              )
              (set! layer-list (cdr layer-list))
            )
          )
        )
        
        ;; Export the modified image
        (file-cur-save RUN-NONINTERACTIVE image (car (gimp-image-get-active-layer image)) output-file output-file)
        
        ;; Clean up
        (gimp-image-delete image)
      )
      (set! filelist (cdr filelist))
    )
  )
)

;; Register the script
(script-fu-register "batch-cursor-to-orange"
                    "Batch Convert Cursors to Orange"
                    "Converts all .cur files from yellow to orange with -23 hue shift"
                    "User"
                    "User"
                    "2025"
                    ""
                    SF-DIRNAME "Input Directory (elevated_cursors)" ""
                    SF-DIRNAME "Output Directory (orange_cursors)" ""
)

(script-fu-menu-register "batch-cursor-to-orange" "<Image>/Filters/Batch")
