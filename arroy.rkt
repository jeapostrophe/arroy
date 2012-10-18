#lang racket/base
(require "lts.rkt"
         racket/function
         racket/list
         racket/match
         racket/unit)

;; Arroy takes an LTS and presents a stepper for it, but it also
;; annotates that options with whether the current player can win.
(define-unit arroy@
  (import lts^)
  (export)

  (define (available s)
    (filter-map
     (λ (m)
       (define ns (next s m))
       (and ns (cons m ns)))
     moves))

  (define (can-win? p s)
    (match (available s)
      [(list)
       (equal? p (winner s))]
      [a
       (ormap (compose (curry can-win? p) cdr) a)]))

  (let loop ([s initial-state])
    (printf "Current state: ~a\n"
            s)
    (match (available s)
      [(list)
       (printf "Winner: ~a\n"
               (winner s))]
      [a
       (printf "Available moves:\n")
       (for ([m*ns (in-list a)]
             [i (in-naturals)])
         (printf "\t~a. ~a\n"
                 i (car m*ns)))
       (printf "Selection: ") (flush-output)
       (define i (read))
       (loop (cdr (list-ref a i)))])))

(provide arroy@)