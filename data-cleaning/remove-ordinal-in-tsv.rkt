; Luke Smith's documents had ordinals, which take up space.
#lang racket

(define (process-bible-file input-file output-file)
  (with-output-to-file output-file #:exists 'replace
    (lambda ()
      (with-input-from-file input-file
        (lambda ()
          (for ([line (in-lines)])
            (let* ([parts (string-split line "\t")]
                   [new-parts (if (>= (length parts) 6) ; Check that lines have enough parts
                                 (list 
                                  (list-ref parts 0)  ; Book name
                                  (list-ref parts 1)  ; Book abbreviation
                                        ; reminds 3rd element here
                                  (list-ref parts 3)  ; Chapter number
                                  (list-ref parts 4)  ; Verse number
                                  (list-ref parts 5)) ; Verse text
                                 #f)])
              (when new-parts
                (displayln (string-join new-parts "\t"))))))))))

; config
(process-bible-file "greek-bible.tsv" "greek-bible2.tsv")