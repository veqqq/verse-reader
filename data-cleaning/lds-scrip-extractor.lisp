(declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))
(defpackage :lds-scrip-extractor
  (:use :cl))

(in-package :lds-scrip-extractor)

(defparameter *book-abbreviations*
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
    ("4 Ne." . "4 ne")))

(defun split-csv-line (line)
  (loop with result = nil
        with current = (make-string-output-stream)
        with in-quotes = nil
        for char across line
        do (case char
             (#\" (setf in-quotes (not in-quotes)))
             (#\, (if in-quotes
                     (write-char char current)
                     (progn 
                       (push (get-output-stream-string current) result)
                       (setf current (make-string-output-stream)))))
             (otherwise (write-char char current)))
        finally (push (get-output-stream-string current) result)
                (return (nreverse result))))

(defun use-custom-abbreviation (book-name)
  (let ((pair (assoc book-name *book-abbreviations* :test #'string=)))
    (if pair
        (cdr pair)
        book-name)))

(defun process-bible-file (input-file output-file)
  (with-open-file (in input-file)
    (with-open-file (out output-file 
                         :direction :output 
                         :if-exists :supersede)
      (loop for line = (read-line in nil nil)
            while line
            do (let* ((parts (split-csv-line line)))
                 (when (string= (nth 10 parts) "BoM")
                   (let ((extracted-parts 
                          (list (nth 5 parts)   ; Book name
                                (use-custom-abbreviation (nth 11 parts))  ; Book
                                (nth 14 parts)  ; Chapter
                                (nth 15 parts)  ; Verse
                                (nth 16 parts)))) ; Verse
                     (format out "~{~A~^	~}~%" extracted-parts))))))))

(defun main ()
  (process-bible-file "lds-scriptures.csv" "BoMormon.tsv"))

(sb-ext:save-lisp-and-die 
  "lds-scrip-extractor"
  :toplevel 'lds-scrip-extractor::main
  :executable t
  :purify t)