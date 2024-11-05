(defpackage :kjv
  (:use :cl)
  (:export :main))

(in-package :kjv)

(defstruct bible-verse
  abbrev
  chapter
  verse
  text)

; Parse string to extract verse
(defun parse-verse-line (line)
  (let* ((parts (uiop:split-string line :separator '(#\Tab)))
         (len (length parts)))
    (if (= len 5)
        (make-bible-verse
         :abbrev (string-downcase (nth 1 parts))
         :chapter (parse-integer (nth 2 parts))
         :verse (parse-integer (nth 3 parts))
         :text (nth 4 parts))
        (progn
          (format t "Skipping non-verse line: ~a~%" line)
          nil))))

;; Load verses at compile time and create a constant
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *kjv-verses*
    (with-open-file (stream "kjv.tsv") ; config
      (loop for line = (read-line stream nil nil)
            while line
            for verse = (parse-verse-line line)
            when verse
            collect verse))))

(defun display-verse (verse)
  (format t "~a~%" (bible-verse-text verse)))

; Allow ge, gen, gene etc. based on the abbreviated ge
(defun string-prefix-p (prefixes string)
  (let ((string (string-downcase string)))
    (some (lambda (prefix)
            (let ((prefix (string-downcase prefix)))
              (and (<= (length prefix) (length string))
                   (string= prefix string :end2 (length prefix)))))
          prefixes)))

;; Process query, display matching verses
(defun process-query (verses query)
  (let* ((parts (uiop:split-string query))
         (book-parts (butlast parts))
         (book-query (string-downcase (if book-parts (format nil "~{~a~^ ~}" book-parts) query))) ; n.b. accepts e.g. `2 Kings` fine
         (chapter-verse (car (last parts)))

         ; Filter verses by book
         (matching-verses
          (remove-if-not
           (lambda (verse)
             (string-prefix-p '("ge" "gen" "gene") ; Add abbreviations here as needed
                              (bible-verse-abbrev verse)))
           verses)))

    (when (null matching-verses)
      (format t "No match found for: ~a~%" book-query)
      (sb-ext:quit :unix-status 1))

    (if (= (length parts) 1)
        ;; Whole book queries: `genesis`
        (mapc #'display-verse matching-verses)

        ;; Handle specific queries: `genesis 1:1` or `genesis 1`
        (let* ((verse-parts (uiop:split-string chapter-verse :separator '(#\:)))
               (chapter (parse-integer (car verse-parts) :junk-allowed t))
               (filtered-verses
                (remove-if-not
                 (lambda (verse)
                   (= (bible-verse-chapter verse) chapter))
                 matching-verses)))

          (if (= (length verse-parts) 2)
              ;; Chapter and verse: `genesis 1:1`
              (let* ((verse-num (parse-integer (cadr verse-parts) :junk-allowed t))
                     (specific-verse
                      (remove-if-not
                       (lambda (verse)
                         (= (bible-verse-verse verse) verse-num))
                       filtered-verses)))
                (if specific-verse
                    (mapc #'display-verse specific-verse)
                    (format t "No verse found for: ~a ~a~%" book-query chapter-verse)))

              ;; Just chapter: `genesis 1`
              (if filtered-verses
                  (mapc #'display-verse filtered-verses)
                  (format t "No chapter found for: ~a ~a~%" book-query chapter)))))))

;; Main entry point
(defun main ()
  (let ((args (rest sb-ext:*posix-argv*)))
    (if (null args)
        (progn
          (format t "Usage: kjv <book> [chapter:verse]~%")
          (sb-ext:quit :unix-status 1))
        (process-query *kjv-verses* (format nil "~{~a~^ ~}" args)))))

;; For building a standalone binary
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :sb-ext)
    (pushnew :sbcl *features*)))

#+sbcl
(sb-ext:save-lisp-and-die
 "kjv" ; config
 :toplevel #'kjv:main
 :executable t)