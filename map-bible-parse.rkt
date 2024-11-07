#lang racket/base
(#%declare #:unsafe)

; #ToDo: add types
; #ToDo: make list of works with similar structure
;    ; Vedas, Quran, Book of Mormon, Buddhist things? Chinese Classic of Poetry
;    ; Stephen Langton added chapters in the 13th c. and Robert Estienne verses in the 16th, which inspired philologists approaching other books

(require racket/string
         racket/cmdline
         racket/list ; drop-right
         (for-syntax racket/base ; for compile time
                     racket/runtime-path
                     racket/port))

;; Logic to parse data and embed it in static binary

; This submodule lets you use defines compile time via for-syntax!
(module verse-struct racket/base
  (#%declare #:unsafe)
  (require racket/string)

  (struct bible-verse (
                       abbrev 
                       chapter
                       verse
                       text
                       ) #:prefab)

  ; Parse string to extract verse
  ; this string splitting in this takes 90% of compile time, especially with the regex match
  (define (parse-verse-line line)
    (define parts (string-split line "\t"))
    (if (= (length parts) 5)
        (bible-verse
         (string-downcase (list-ref parts 1)) ; abbreviation
         (string->number (list-ref parts 2)) ; chapter
         (string->number (list-ref parts 3)) ; verse 
         (list-ref parts 4)) ; text
        (begin
          (printf "Skipping non-verse line: ~a\n" line)
          #f)))
  ; tried removing begin, replacing if with and
  ; no change in speed, but worse potential error handling

  (provide bible-verse parse-verse-line
           bible-verse-abbrev bible-verse-chapter 
           bible-verse-verse bible-verse-text))

; Require the verse struct both normally and for-syntax
(require 'verse-struct
         (for-syntax 'verse-struct)) ; lsp falsely says not required https://github.com/racket/drracket/issues/692

; Compile-time parse and hashmap building
(begin-for-syntax
  (define-runtime-path kjv-path "kjv.tsv")
  (define kjv-verses
    (for/list ([line (in-list (with-input-from-file kjv-path port->lines))]
               #:when (parse-verse-line line))
      (parse-verse-line line)))
  (define verse-maps
    (let ([book-map (make-hash)])
      (for ([verse kjv-verses])
        (define abbrev (bible-verse-abbrev verse))
        (define chapter (bible-verse-chapter verse))
        (define chapter-map 
          (hash-ref! book-map abbrev (lambda () (make-hash))))
        (define chapter-verses 
          (hash-ref! chapter-map chapter (lambda () '())))
        (hash-set! chapter-map chapter 
                   (cons verse chapter-verses)))
      book-map)))

; Make parsed verses available at runtime via macro
(define-syntax (get-verse-maps stx)
  (syntax-case stx ()
    [(_) #`(quote #,verse-maps)]))

;; Logic Query etc. Parsing and Handling


(define (display-verse verse)
  (display (bible-verse-text verse))
  " ")

; Process query, display matching verses
; So far, making this functional goes from .250 to 320ms
(define (process-query query)
  (define verse-maps (get-verse-maps))
  (define parts (string-split query " "))
  (define book-query
    (string-downcase
     (if (string->number (first parts))
         (string-append (first parts) (second parts))
         (first parts))))

  ; (define chapter-verse (if (> (length parts) 1) (last parts) #f))
  (define chapter-verse 
    (and (> (length parts) 1)
         (let ([last-part (last parts)])
           (and (or (string->number last-part)  ; pure number like "1"
                    (regexp-match? #rx"[0-9]" last-part)) ; contains number like "1:1"
                last-part))))

  ; Filter verses by book
  (define matching-book-map
    (for/first ([(abbrev chapter-map) (in-hash verse-maps)]
                #:when (string-prefix? book-query abbrev))
      chapter-map))
  (when (not matching-book-map)
    (printf "No match found for: ~a\n" book-query)
    (exit 1))

  ; Whole book: "genesis"
  (if (not chapter-verse)
      (begin
        (for* ([(chapter verses) (in-hash matching-book-map)]
               [verse (in-list verses)])
          (display-verse verse))
        (newline))
      ; Handle specific queries: `genesis 1:1` or `genesis 1`
      (let* ((verse-parts (string-split chapter-verse ":"))
             (chapter (string->number (first verse-parts)))
             (chapter-verses (hash-ref matching-book-map chapter '())))
        (cond
          ; Chapter and verse: `genesis 1:1`
          [(= (length verse-parts) 2)
           (define verse-num (string->number (second verse-parts)))
           (for ([verse (in-list chapter-verses)]
                 #:when (= (bible-verse-verse verse) verse-num))
             (display-verse verse))
           (newline)]
          ; Just chapter: `genesis 1`
          [else
           (for-each display-verse chapter-verses)
           (newline)]))))

(module+ main
  (command-line
   #:program "kjv"
   #:args query-list
   (when (null? query-list)
     (printf "Usage: kjv <book> [chapter:verse]\n")
     (exit 1))
   (process-query (string-join query-list " "))))
