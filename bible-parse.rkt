#lang racket/base

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
  (require racket/string)
  
  ; Changed to prefab for compile-time support
  (struct bible-verse (
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
         ;; note, no book
         (string-downcase (list-ref parts 1)) ; abbreviation
         (string->number (list-ref parts 2)) ; chapter
         (string->number (list-ref parts 3)) ; verse
         (list-ref parts 4))              ; text
        (begin
          (printf "Skipping non-verse line: ~a\n" line)
          #f)))
  
  (provide bible-verse bible-verse? parse-verse-line
           bible-verse-abbrev 
           bible-verse-chapter bible-verse-verse 
           bible-verse-text))

; Require the verse struct both normally and for-syntax
(require 'verse-struct
         (for-syntax 'verse-struct)) ; lsp falsely says not required

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

(define (display-verse verse)
  ; (printf "~a:~a ~a\n"
    (printf "~a\n"
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

;;

(module+ test
  (require rackunit racket/file racket/port)

  (define (create-test-file)
    (define temp-file (make-temporary-file "test-verses-~a.tsv"))
    (with-output-to-file temp-file
      #:exists 'replace
      (λ ()
        (printf "id\tabbrev\tchapter\tverse\ttext\n")
        (printf "1\t1 ne\t1\t5\tWherefore it came to pass that my father, Lehi, as he went forth prayed unto the Lord, yea, even with all his heart, in behalf of his people.\n")
        (printf "2\tmos\t26\t14\tAnd it came to pass that after he had poured out his whole soul to God, the voice of the Lord came to him, saying:\n")
        (printf "3\t3 ne\t11\t26\tAnd then shall ye immerse them in the water, and come forth again out of the water.\n")
        (printf "4\th\t11\t37\tAnd it came to pass in the eighty and fifth year they did wax stronger and stronger in their pride, and in their wickedness; and thus they were ripening again for destruction.\n")))
    temp-file)
  
  (define test-file (create-test-file))

  (test-case "parse-verse-line correctly parses valid input"
    (define test-line "1\t1 ne\t1\t5\tWherefore it came to pass that my father, Lehi, as he went forth prayed unto the Lord, yea, even with all his heart, in behalf of his people.\n")
    (define result (parse-verse-line test-line))
    (check-equal? (bible-verse-abbrev result) "1 ne")
    (check-equal? (bible-verse-chapter result) 1)
    (check-equal? (bible-verse-verse result) 5)
    (check-equal? (bible-verse-text result) "Wherefore it came to pass that my father, Lehi, as he went forth prayed unto the Lord, yea, even with all his heart, in behalf of his people.\n"))

  (test-case "display-verse formats output correctly"
    (define output-verse (bible-verse "1 ne" 1 5 "Test verse text"))
    (define expected-output "Test verse text\n")
    
    (check-equal? 
     (with-output-to-string (λ () (display-verse output-verse)))
     expected-output))
  
  (delete-file test-file))