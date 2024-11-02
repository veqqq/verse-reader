#lang racket/base
(require racket/string
         racket/cmdline
         racket/list ; drop-right
         (for-syntax racket/base ; for compile time
                     racket/runtime-path
                     racket/port))

; Embed file content as syntax object for compiletime
(begin-for-syntax
  (define-runtime-path kjv-path "kjv.tsv")
  (define kjv-content
    (with-input-from-file kjv-path
      (lambda () (port->lines)))))  ; Changed from port->string to port->lines

; Make data runtime available via macro
(define-syntax get-kjv-data
  (lambda (stx)
    (syntax-case stx ()
      [(_) #`(quote #,kjv-content)])))

; Bible verse struct
(struct bible-verse (book abbrev chapter verse text) #:transparent)

; Parse string to extract verse
(define (parse-verse-line line)
  (define parts (string-split line "\t"))
  (if (= (length parts) 6)
      (bible-verse
       (list-ref parts 0)               ; book
       (string-downcase (list-ref parts 1)) ; abbreviation (to help search)
       (string->number (list-ref parts 3)) ; chapter
       (string->number (list-ref parts 4)) ; verse
       (list-ref parts 5))               ; text
      (begin
        (printf "Skipping non-verse line: ~a\n" line)
        #f)))

; Load verses from saved lines
(define (load-verses-from-lines lines)  ; Changed from load-verses-from-string to load-verses-from-lines
  (filter values
          (for/list ([line (in-list lines)])  ; No need to split string anymore
            (parse-verse-line line))))

; Match book names or abbrv inputs
(define (book-matches? verse book-query)
  (define query (string-downcase book-query))
  (or (string-prefix? query (string-downcase (bible-verse-book verse)))
      (string-prefix? query (string-downcase (bible-verse-abbrev verse)))))

(define (display-verse verse)
  (printf "~a ~a:~a ~a\n"
          (bible-verse-book verse)
          (bible-verse-chapter verse)
          (bible-verse-verse verse)
          (bible-verse-text verse)))

; Process query, display matching verses
(define (process-query verses query)
  (define parts (string-split query " "))
  (define book-query (string-join (if (> (length parts) 1) (drop-right parts 1) parts) " "))
  (define chapter-verse (if (> (length parts) 1) (last parts) #f))
  
  ; Filter verses by book
  (define matching-verses
    (filter (λ (verse) (book-matches? verse book-query)) verses))
  (when (null? matching-verses)
    (printf "No match found for: ~a\n" book-query)
    (exit 1))
  
  (cond
    ; Whole book queries: `genesis`
    [(not chapter-verse)
     (for-each display-verse matching-verses)]
    ; Handle specific queries: `genesis 1:1` or `genesis 1`
    [else
     (define verse-parts (string-split chapter-verse ":"))
     (define chapter (string->number (car verse-parts)))
     (define filtered-verses
       (filter (λ (verse) (= (bible-verse-chapter verse) chapter))
               matching-verses))
     (cond
       ; Chapter and verse: `genesis 1:1`
       [(= (length verse-parts) 2)
        (define verse-num (string->number (cadr verse-parts)))
        (define specific-verse
          (filter (λ (verse) (= (bible-verse-verse verse) verse-num))
                  filtered-verses))
        (for-each display-verse specific-verse)]
       ; Just chapter: `genesis 1`
       [else
        (for-each display-verse filtered-verses)])]))

(module+ main
  (define verses (load-verses-from-lines (get-kjv-data)))  ; Changed to use load-verses-from-lines
  (command-line
    #:program "kjv" ; since file is hardcoded #ToDo: deploy binaries for other books y/n?
    #:args query-list
    (when (null? query-list)
      (printf "Usage: kjv <book> [chapter:verse]\n")
      (exit 1))
    (process-query verses (string-join query-list " "))))