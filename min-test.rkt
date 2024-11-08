; #lang typed/racket/base
#lang racket/base

; somehow using typed racket with type annotations makes initialization far slower

; (: add (-> Void))
(define (add)
  ; (: args (Vectorof String))
  (define args (current-command-line-arguments))

  (define n1 (string->number (vector-ref args 0)))
  (define n2 (string->number (vector-ref args 1)))
; ; (define n1 (string->number (vector-ref (current-command-line-arguments) 0) number?))
; ; (define n2 (string->number (vector-ref (current-command-line-arguments) 1) number?))
  (display (+ n1 n2)))

(add)