#lang racket

; extracts from: https://github.com/mormon-documentation-project/lds-scriptures/blob/master/lds-scriptures.csv


#| example data:

3,74,1279,33405,Book of Mormon,Mosiah,The Book of Mormon,The Book of Mosiah,Another Testament of Jesus Christ,"",BoM,Mosiah,bm,mosiah,24,20,"And Alma and his people departed into the wilderness; and when they had traveled all day they pitched their tents in a valley, and they called the valley Alma, because he led their way in the wilderness.",Mosiah 24:20,Mosiah 24:20

3,80,1407,37208,Book of Mormon,Ether,The Book of Mormon,The Book of Ether,Another Testament of Jesus Christ,"",BoM,Ether,bm,ether,4,2,And after Christ truly had showed himself unto his people he commanded that they should be made manifest.,Ether 4:2,Ether 4:2


The Son of Nephi, who was the Son of Helaman",BoM,3 Ne.,bm,3-ne,6,11,"For there were many merchants in the land, and also many lawyers, and many officers.",3 Nephi 6:11,3 Ne. 6:11
3,77,1369,36195,Book of Mormon,3 Nephi,The Book of Mormon,The Third Book of Nephi,Another Testament of Jesus Christ,"The Book of Nephi
The Son of Nephi, who was the Son of Helaman",BoM,3 Ne.,bm,3-ne,6,12,"And the people began to be distinguished by ranks, according to their riches and their chances for learning; yea, some were ignorant because of their poverty, and others did receive great learning because of their riches.",3 Nephi 6:12,3 Ne. 6:12
3,77,1369,36196,Book of Mormon,3 Nephi,The Book of Mormon,The Third Book of Nephi,Another Testament of Jesus Christ,"The Book of Nephi

Who is the son of Nephi, one of the disciples of Jesus Christ",BoM,4 Ne.,bm,4-ne,1,1,"And it came to pass that the thirty and fourth year passed away, and also the thirty and fifth, and behold the disciples of Jesus had formed a church of Christ in all the lands round about. And as many as did come unto them, and did truly repent of their sins, were baptized in the name of Jesus; and they did also receive the Holy Ghost.",4 Nephi 1:1,4 Ne. 1:1
3,78,1394,36836,Book of Mormon,4 Nephi,The Book of Mormon,The Fourth Book of Nephi,Another Testament of Jesus Christ,"The Book of Nephi
Who is the son of Nephi, one of the disciples of Jesus Christ",BoM,4 Ne.,bm,4-ne,1,2,"And it came to pass in the thirty and sixth year, the people were all converted unto the Lord, upon all the face of the land, both Nephites and Lamanites, and there were no contentions and disputations among them, and every man did deal justly one with another.",4 Nephi 1:2,4 Ne. 1:2
3,78,1394,36837,Book of Mormon,4 Nephi,The Book of Mormon,The Fourth Book of Nephi,Another Testament of Jesus Christ,"The Book of Nephi


4,82,1433,37800,Doctrine and Covenants,Doctrine and Covenants,The Doctrine and Covenants,The Doctrine and Covenants,"",of the Church of Jesus Christ of Latter-day Saints,D&C,D&C,dc,dc,5,25,"And then he shall say unto the people of this generation: Behold, I have seen the things which the Lord hath shown unto Joseph Smith, Jun., and I know of a surety that they are true, for I have seen them, for they have been shown unto me by the power of God and not of man.",Doctrine and Covenants 5:25,D&C 5:25

|#

#| data format:

3,81,1427,37664,Book of Mormon,Moroni,The Book of Mormon,The Book of Moroni,Another Testament of Jesus Christ,"",BoM,Moro.,bm,moro,9,18,"O
1  2   3    4     5              6      7                      8              9                                10 11   12   13  14  15 16 17

17 text
18 book and verse
19 book and verse, but book abreviated
|#

(require csv-reading)
(define (split-csv-line line)
  (call-with-input-string line
    (λ (in)
      (car (csv->list in)))))

(define (process-bible-file input-file output-file)
  (with-output-to-file output-file #:exists 'replace
    (lambda ()
      (with-input-from-file input-file
        (lambda ()
          (for ([line (in-lines)])
            (let* ([parts (split-csv-line line)])
              (when (equal? (list-ref parts 10) "BoM") ; #config
                (let ([extracted-parts (list 
                                      (list-ref parts 5)   ; Book name
                                      (use-custom-abbreviation (list-ref parts 11))  ; Book abbreviation
                                      (list-ref parts 14)  ; Chapter number
                                      (list-ref parts 15)  ; Verse number
                                      (list-ref parts 16)  ; Verse text
                                     )])
                  (displayln (string-join extracted-parts "\t")))))))))))

(define book-abbreviations
  '(("Ether" . "et")
    ("Alma" . "a")
    ("Hel." . "h")
    ("Omni" . "o")
    ("W of M" . "w")
    ("Jarom" . "jar")
    ("Enos" . "en")
    ("Morm." . "morm")
    ("Mosiah" . "mos")
    ("Moro." . "moro")
    ("Jacob" . "jac")
    ("1 Ne." . "1 ne")
    ("2 Ne." . "2 ne")
    ("3 Ne." . "3 ne")
    ("4 Ne." . "4 ne")
    ))

; Just hardcoded, data's too inconsistent either way
; (define (normalize-book-name str)
;   (string-downcase
;    (string-trim
;     (regexp-replace* #rx"\\." str "") ; Remove all periods
;     #:left? #t
;     #:right? #t)))

(define (use-custom-abbreviation book-name)
  ; (define normalized-name (normalize-book-name book-name))
  (match (assoc book-name book-abbreviations) ; book-name -> normalized->name ; and below
    [(cons _ abbr) abbr]  ; if abrv
    [_ book-name])) ; original if no abrv, actually Nephis need period so not used

(process-bible-file "lds-scriptures.csv" "BoMormon.tsv") ; #config

;;
(module+ test
  (require rackunit racket/file csv-reading)

  ; This will be useful for my own csv parser using regexes
  ; (regexp-split #px",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)" line)
  (test-case "split-csv-line splits CSV line correctly"
    (define test-line #<<END
3,67,1190,31107,Book of Mormon,1 Nephi,The Book of Mormon,The First Book of Nephi,**Another Testament of Jesus Christ,**His Reign and Ministry,BoM,1 Ne.,bm,1-ne,1,5,"Wherefore it came to pass that my father, Lehi, as he went forth prayed unto the Lord, yea, even with all his heart, in behalf of his people.",1 Nephi 1:5,**1 Ne. 1:5**
END
)
    (define result (split-csv-line test-line))
    
    ; Structure tests
    (check-not-false result "Result should not be false")
    (check-pred list? result "Result should be a list")
    (check-equal? (length result) 19 "Should have 19 fields")
    
    ; Test fields
    (let ([expected-fields 
           (list "3" "67" "1190" "31107" "Book of Mormon" "1 Nephi" 
                 "The Book of Mormon" "The First Book of Nephi"
                 "**Another Testament of Jesus Christ" "**His Reign and Ministry"
                 "BoM" "1 Ne." "bm" "1-ne" "1" "5"
                 "Wherefore it came to pass that my father, Lehi, as he went forth prayed unto the Lord, yea, even with all his heart, in behalf of his people."
                 "1 Nephi 1:5" "**1 Ne. 1:5**")])
      (for ([expected expected-fields]
            [actual result]
            [index (in-naturals)])
        (check-equal? actual expected 
                     (format "Field ~a should be '~a'" index expected)))))

  (test-case "use-custom-abbreviation returns correct abbreviations"
    (check-equal? (use-custom-abbreviation "3 Ne.") "3 ne" 
                  "Should convert '1 Ne.' to '1 ne'")
    (check-equal? (use-custom-abbreviation "Mosiah") "mos"
                  "Should convert 'Mosiah' to 'mos'")
    (check-equal? (use-custom-abbreviation "Hel.") "h"
                  "Should convert 'Hel.' to 'h'"))

  ; Test file processing
  (test-case "process-bible-file generates correct output"

    (define temp-output-file (make-temporary-file "test-output-~a.tsv"))
    (define temp-input-file (make-temporary-file "test-verses-~a.csv"))

    (define example-text #<<END
3,67,1190,31107,Book of Mormon,1 Nephi,The Book of Mormon,The First Book of Nephi,**Another Testament of Jesus Christ,**His Reign and Ministry,BoM,1 Ne.,bm,1-ne,1,5,"Wherefore it came to pass that my father, Lehi, as he went forth prayed unto the Lord, yea, even with all his heart, in behalf of his people.",1 Nephi 1:5,**1 Ne. 1:5**
\n3,74,1281,33448,Book of Mormon,Mosiah,The Book of Mormon,The Book of Mosiah,**Another Testament of Jesus Christ,**"",BoM,Mosiah,bm,mosiah,26,14,"And it came to pass that after he had poured out his whole soul to God, the voice of the Lord came to him, saying:",Mosiah 26:14,**Mosiah 26:14**
\n3,77,1374,36331,Book of Mormon,3 Nephi,The Book of Mormon,The Third Book of Nephi,Another Testament of Jesus Christ,"The Book of Nephi The Son of Nephi, who was the Son of Helaman",BoM,3 Ne.,bm,3-ne,11,26,"And then shall ye immerse them in the water, and come forth again out of the water.",3 Nephi 11:26,3 Ne. 11:26
\n3,76,1358,35910,Book of Mormon,Helaman,The Book of Mormon,The Book of Helaman,Another Testament of Jesus Christ,"",BoM,Hel.,bm,hel,11,37,"And it came to pass in the eighty and fifth year they did wax stronger and stronger in their pride, and in their wickedness; and thus they were ripening again for destruction.",Helaman 11:37,Hel. 11:37
END
)

   (with-output-to-file temp-input-file #:exists 'replace
      (λ ()
        (displayln example-text )))

    (check-not-exn
     (λ () (process-bible-file temp-input-file temp-output-file))
     "File processing should not throw an exception")

    (check-true (file-exists? temp-output-file)
                "Output file should exist")
    
    (define output-lines (file->lines temp-output-file))
    (check-equal? (length output-lines) 4
                  "Output should contain 4 lines")

  (define expected-lines
      (list
       "1 Nephi\t1 ne\t1\t5\tWherefore it came to pass that my father, Lehi, as he went forth prayed unto the Lord, yea, even with all his heart, in behalf of his people."
       "Mosiah\tmos\t26\t14\tAnd it came to pass that after he had poured out his whole soul to God, the voice of the Lord came to him, saying:"
       "3 Nephi\t3 ne\t11\t26\tAnd then shall ye immerse them in the water, and come forth again out of the water."
       "Helaman\th\t11\t37\tAnd it came to pass in the eighty and fifth year they did wax stronger and stronger in their pride, and in their wickedness; and thus they were ripening again for destruction."
       ))

    ; Check line structure
    (for ([actual output-lines]
          [expected expected-lines]
          [line-num (in-naturals 1)])
      ; Check correct fields
      (define actual-fields (string-split actual "\t"))
      (define expected-fields (string-split expected "\t"))
      
      (check-equal? (length actual-fields) (length expected-fields)
                    (format "Line ~a should have correct number of fields" line-num))
      
      (for ([actual-field actual-fields]
            [expected-field expected-fields]
            [field-num (in-naturals 1)])
        (check-equal? actual-field expected-field
                     (format "Line ~a, Field ~a should match expected content" 
                            line-num field-num))))

    (delete-file temp-input-file)
    (delete-file temp-output-file))
)