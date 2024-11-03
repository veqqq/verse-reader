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

#| approach
skip front matter, which can differ like:
The Son of Nephi, who was the Son of Helaman",
or
3,74,1279,33405,Book of Mormon,Mosiah,The Book of Mormon,The Book of Mosiah,Another Testament of Jesus Christ,""

but instead start at the
"BoM"

BoM,Ether,bm,ether,4,2,And after Christ truly had showed himself unto his people he commanded that they should be made manifest.,Ether 4:2,Ether 4:2
1 2 3 4 5 6 7th element is the text, 7th is the book and verse, 8th is also book and verse?
BoM,3 Ne.,bm,3-ne,6,11,"For there were many merchants in the land, and also many lawyers, and many officers.",3 Nephi 6:11,3 Ne. 6:11
1 2 3 4 5 6 7th is the text ...in quotes!? 8th is book and verse, 9th is book and verse but with book abreviated.

|#



;; 3,81,1427,37664,Book of Mormon,Moroni,The Book of Mormon,The Book of Moroni,Another Testament of Jesus Christ,"",BoM,Moro.,bm,moro,9,18,"O
; 1  2   3    4     5              6      7                      8              9                                10 11   12   13  14  15 16 17

#|
17 text
18 book and verse
19 book and verse, but book abreviated
|#

; (define (split-csv-line line) ; keeping as a hilarious monument and reminder to explore the libraries first
;   (regexp-split #px",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)" line)) ; #ToDo: replace with csv lib

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
  (match (assoc book-name book-abbreviations) ; or use normalized-name here and below...
    [(cons _ abbr) abbr]  ; if abrv
    [_ book-name])) ; original if no abrv, actually Nephis need period so not used

(process-bible-file "lds-scriptures.csv" "BoMormon.tsv") ; #config