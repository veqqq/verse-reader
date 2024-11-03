#lang racket/base

; #ToDo: add types
; #ToDo: make list of works with similar structure
;    ; Vedas, Quran, Book of Mormon, Buddhist things? Chinese Classic of Poetry
;    ; Stephen Langton added chapters in the 13th c. and Robert Estienne verses in the 16th, which inspired philologists approaching other books
; #ToDo: find/produce equivalently formated texts y/n?
; #ToDo: expand to things without this format y/n?
; #ToDo: maybe go in a direction with poetry, if I can find a word-> phoneme dictionary. But data cleaning issues due to modern printings removing contractions like "th'art" etc. Hmhmhm

(require racket/string
         racket/cmdline
         racket/list ; drop-right
         (for-syntax racket/base ; for compile time
                     racket/runtime-path
                     racket/port))

;; Logic to parse data and embed it in static binary

; I might remove this later. Unsure what benefit submodule gives
(module verse-struct racket/base
  (require racket/string)
  
  ; Changed to prefab for compile-time support
  (struct bible-verse (
                      ; [book #:mutable]
                      [abbrev #:mutable]
                      [chapter #:mutable]
                      [verse #:mutable]
                      [text #:mutable])
    #:prefab)
  
  ; Parse string to extract verse
  (define (parse-verse-line line)
    (define parts (string-split line "\t"))
    (if (= (length parts) 5)
        (bible-verse
        ;  (list-ref parts 0)               ; book
         (string-downcase (list-ref parts 1)) ; abbreviation
         (string->number (list-ref parts 2)) ; chapter,  skipped 2, it's ordinal of books in tsv #ToDo: eliminate ordinals from tsv and change this
         (string->number (list-ref parts 3)) ; verse
         (list-ref parts 4))              ; text
        (begin
          (printf "Skipping non-verse line: ~a\n" line)
          #f)))
  
  (provide bible-verse bible-verse? parse-verse-line
          ;  bible-verse-book
           bible-verse-abbrev 
           bible-verse-chapter bible-verse-verse 
           bible-verse-text))

; Require the verse struct both normally and for-syntax
(require 'verse-struct
         (for-syntax 'verse-struct))

; Compile-time parse
(begin-for-syntax
  (define-runtime-path kjv-path "kjv.tsv")
  (define kjv-verses
    (filter values
            (for/list ([line (in-list (with-input-from-file kjv-path port->lines))])
              (parse-verse-line line)))))

; Make parsed verses available at runtime via macro
(define-syntax get-kjv-verses
  (lambda (stx)
    (syntax-case stx ()
      [(_) #`(quote #,kjv-verses)])))


;; Logic Query etc. Parsing and Handling

; ; Match book names or abbrv inputs
; (define (book-matches? verse book-query)
;   (define query (string-downcase book-query))
;   ; (or (string-prefix? query (string-downcase (bible-verse-book verse)))
;   ;     (string-prefix? query (string-downcase (bible-verse-abbrev verse))))
;       (string-prefix? query (bible-verse-abbrev verse)))

(define (display-verse verse)
  ; (printf "~a ~a:~a ~a\n"
    (printf "~a\n"
          ; (bible-verse-book verse)
          ; (bible-verse-chapter verse)
          ; (bible-verse-verse verse)
          (bible-verse-text verse)))

; Process query, display matching verses
(define (process-query verses query)
  (define parts (string-split query " "))
  (define book-query (string-join (if (> (length parts) 1) (drop-right parts 1) parts) " ")) ; n.b. accepts e.g. `2 Kings` fine
  (define chapter-verse (if (> (length parts) 1) (last parts) #f))
  
  ; Filter verses by book
  (define matching-verses
    (filter (λ (verse) (string-prefix? (string-downcase book-query) (bible-verse-abbrev verse))) verses))
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
  (define verses (get-kjv-verses))
  (command-line
   #:program "kjv" ; since file is hardcoded
   #:args query-list
   (when (null? query-list)
     (printf "Usage: kjv <book> [chapter:verse]\n")
     (exit 1))
   (process-query verses (string-join query-list " "))))
