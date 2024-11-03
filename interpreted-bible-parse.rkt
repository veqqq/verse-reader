; this is for testing new data manually

#lang racket/base

(require racket/string
  racket/cmdline
    racket/list) ; drop-right

; Bible verse struct
(struct bible-verse (book abbrev chapter verse text) #:transparent)

; Parse TSV to extract verse
(define (parse-verse-line line)
  (define parts (string-split line "\t"))
  (if (= (length parts) 5)
      (bible-verse
       (list-ref parts 0)               ; book
       (string-downcase (list-ref parts 1)) ; abbreviation (to help search)
       (string->number (list-ref parts 2)) ; chapter
       (string->number (list-ref parts 3)) ; verse
       (list-ref parts 4))               ; text
      (begin
        (printf "Skipping non-verse line: ~a\n" line) ; help find dirty data. Luke Smith's file had 1 weird line (in the apocrapha, "son of Sirach" or such. I eliminated it instead of fixing.)
        #f)))

; Load verses from embedded TSV
(define (load-verses filename)
  (with-input-from-file filename
    (λ ()
      (filter values
              (for/list ([line (in-lines)])
                (parse-verse-line line))))))

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
  (define verses (load-verses "kjv.tsv"))
    (command-line
   #:program "kjv" ; since file is hardcoded #ToDo: deploy binaries for other books y/n?
   #:args query-list
   (when (null? query-list)
     (printf "Usage: kjv <book> [chapter:verse]\n")
     (exit 1))
   (process-query verses (string-join query-list " "))))