#lang racket/base

; (: main (-> Void)) ; ...not using types makes it 5x faster and 20mb smaller?!
(define (main)
  (define args (vector->list (current-command-line-arguments)))
  (let ([n1 (string->number (car args))]
        [n2 (string->number (cadr args))])
    (if (and n1 n2)
        (display (+ n1 n2))
        (display "Error: Invalid number"))))

(main)