#lang racket/base

(require "arroy.rkt"
         racket/list
         racket/match
         racket/set
         racket/unit
         racket/function
         racket/file)

(define-unit ghost@
  (import)
  (export lts^)
  
  (define dict (for/fold ([dict (set)])
                 ([word (in-list (filter (λ (s) (not (or (regexp-match "'" s) ; no apostrophes
                                                         (regexp-match #rx"[A-Z]" s)))) ; no proper nouns
                                         (file->lines #:mode 'text "/usr/share/dict/words")))])
                 (set-add dict word)))
  
  (define (forms-word? letters)
    (set-member? dict (list->string (reverse letters))))
  
  (define (next-turn i n)
    (remainder (add1 i) n))
  
  (define (last-turn i n)
    (remainder (+ (sub1 i) n) n))
  
  (struct middle (letters turn n) #:transparent)
  (struct challenged (letters er ee) #:transparent)
  (struct defending (letters er ee) #:transparent)
  (struct end (winner) #:transparent) ; reward calling people out
  
  (struct challenge () #:transparent)
  (struct declare () #:transparent)
  (struct forfeit () #:transparent)
  (struct add (letter) #:transparent)
  
  (define moves (list* (challenge)
                       (declare)
                       (forfeit)
                       (for/list ([c (in-list (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))])
                         (add c))))
  
  (define (initial-state n)
    (and (> n 1) (middle empty 0 n)))
  
  (define next
    (match-lambda*
      ;; If there are letters, challenge.
      [(list (middle letters turn n) (challenge))
       (and (> (length letters) 0) (challenged letters turn (last-turn turn n)))]
      ;; If there are more than two letters, declare.
      [(list (middle letters turn n) (declare))
       (and (> (length letters) 2) (if (forms-word? letters)
                                       (end n)
                                       (end (last-turn turn n))))]
      ;; If adding letters, add a letter.
      [(list (middle letters turn n) (add c))
       (middle (cons c letters) (next-turn turn n) n)]
      ;; If challenged, add a letter
      [(list (challenged letters er ee) (add c))
       (defending (cons c letters) er ee)]
      ;; challenged person forfeits
      [(list (challenged letters er ee) (forfeit))
       (end er)]
      ;; defending person adds letter
      [(list (defending letters er ee) (add c))
       (defending (cons c letters) er ee)]
      ;; 
      [(list (defending letters er ee) (forfeit))
       (end er)]
      [(list (defending letters er ee) (declare))
       (if (forms-word? letters)
           (end ee)
           (end er))]
      ;; Or you can't do anything
      [_
       #f]))
  
  (define winner
    (match-lambda
      [(end w) w]
      [_ #f]))
  
  (define player
    (match-lambda
      [(middle _ turn _) turn]
      [(challenged _ _ ee) ee]
      [(defending _ _ ee) ee]
      [(end w) w])))

(module+ main
  (invoke-unit/infer (link ghost@ arroy@)))
