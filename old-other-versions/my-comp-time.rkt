#lang racket/base

; #ToDo: add types
; #ToDo: make list of works with similar structure
;    ; Vedas, Quran, Book of Mormon, Buddhist things? Chinese Classic of Poetry
;    ; Stephen Langton added chapters in the 13th c. and Robert Estienne verses in the 16th, which inspired philologists approaching other books
; #ToDo: find/produce equivalently formated texts y/n?
; #ToDo: expand to things without this format y/n?
; #ToDo: maybe go in a direction with poetry, if I can find a word-> phoneme dictionary. But data cleaning issues due to modern printings removing contractions like "th'art" etc. Hmhmhm

; Bible verse struct
(struct bible-verse (book abbrev chapter verse text) #:transparent)

(require racket/string
         racket/cmdline
         racket/list ; drop-right
         (for-syntax racket/base ; for compile time
                     racket/runtime-path
                     racket/port
                     syntax/parse
                     racket/string))

; Embed file content as syntax object for compiletime
(begin-for-syntax
  (define-runtime-path kjv-path "kjv.tsv")

  (define (parse-verse-line line)
    (define parts (string-split line "\t"))
    (and (= (length parts) 6)
         (list (list-ref parts 0)               ; book
               (string-downcase (list-ref parts 1)) ; abbreviation
               (string->number (list-ref parts 3)) ; chapter
               (string->number (list-ref parts 4)) ; verse
               (list-ref parts 5))))            ; text
  ; Compile time parse (reduce execution speed)
  (define kjv-parsed-data
    (filter values
            (for/list ([line (in-list (string-split (with-input-from-file kjv-path port->string) "\n"))])
              (parse-verse-line line)))))
; Make data rutime availible via macro
(define-syntax (get-kjv-data stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([(verse-data ...)
                    (for/list ([verse kjv-parsed-data])
                      #`(bible-verse #,@verse))])
       #'(list verse-data ...))]))

; Match book names or abbrv inputs (abr's given in TSV)
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
  (define book-query (string-join (if (> (length parts) 1) (drop-right parts 1) parts) " ")) ; n.b. accepts e.g. `2 Kings` fine
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
  (define verses (get-kjv-data))
  (command-line
   #:program "kjv" ; since file is hardcoded #ToDo: deploy binaries for other books y/n?
   #:args query-list
   (when (null? query-list)
     (printf "Usage: kjv <book> [chapter:verse]\n")
     (exit 1))
   (process-query verses (string-join query-list " "))))
