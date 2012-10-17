#lang racket/base
(require "arroy.rkt"
         "lts.rkt"
         racket/match
         racket/unit)

;; Guess a number between 0 and 9
(define-unit guess@
  (import)
  (export lts^)

  ;; You're either choosing, guessing, or the game is over
  (struct choose () #:transparent)
  (struct guess (i) #:transparent)
  (struct won () #:transparent)
  (struct lost () #:transparent)

  ;; You always choose first
  (define initial-state
    (choose))

  ;; You can only pick a number
  (struct pick (i) #:transparent)

  (define moves
    (for/list ([i (in-range 10)])
      (pick i)))

  (define next
    (match-lambda*
     ;; If you are choosing, then a pick results in a guess
     [(list (choose) (pick i))
      (guess i)]
     ;; If you pick the guess, then you won
     [(list (guess i) (pick i))
      (won)]
     ;; Or you lost
     [(list (guess i) (pick j))
      (lost)]
     ;; Or you can't do anything
     [_
      #f]))

  (define winner
    (match-lambda
     [(won) 'guesser]
     [(lost) 'picker]))

  (define player
    (match-lambda
     [(choose) 'picker]
     [(guess _) 'guesser]
     [x (winner x)])))

(module+ main
  (invoke-unit/infer (link guess@ arroy@)))