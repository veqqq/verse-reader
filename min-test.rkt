#lang racket/base
(require racket/cmdline)

(define (process-query a b)
  (+ (string->number a) (string->number b)))
  
(module+ main
  (command-line
#:program "calc"
#:args (a b)
    (when (or (null? a) (null? b))
      (printf "Usage: adder num num\n")
      (exit 1))
    (printf "~a\n"
            (process-query a b))))