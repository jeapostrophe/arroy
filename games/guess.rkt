#lang racket/base
(require "../lts.rkt"
         racket/match
         racket/unit)

;; Guess a number between 0 and 9

;; You're either choosing, guessing, or the game is over
(struct state ())
(struct choose state ())
(struct guess state (i))
(struct won state ())
(struct lost state ())

;; You always choose first
(define (make-initial-state players)
  (and (= players 2)
       (choose)))

;; You can only pick a number
(struct move ())
(struct pick move (i))

(define moves
  (for/list ([i (in-range 10)])
    (pick i)))

;; XXX Make this a utility or a unit that takes "moves" and produces
;; "available"
(define (available s p)
  (filter (λ (m) (next s p m)) moves))

(define (score s p)
  (match* (s p)
    [((won) 0) 0]
    [((won) 1) 1]
    [((lost) 0) 1]
    [((lost) 1) 0]
    [(_ _) 0]))

(define (render s-or-m p)
  (match* (s-or-m p)
    [((choose) 0)
     "Choose a number between 0 and 9"]
    [((guess _) 1)
     "Guess a number between 0 and 9"]
    [((pick i) _)
     (format "Pick ~a" i)]))

(define next
  (match-lambda*
   ;; If you are choosing, then a pick results in a guess
   [(list (choose) 0 (pick i))
    (guess i)]
   ;; If you pick the guess, then you won
   [(list (guess i) 1 (pick i))
    (won)]
   ;; Or you lost
   [(list (guess i) 1 (pick j))
    (lost)]
   ;; Or you can't do anything
   [_
    #f]))

(define-lts
  "Guess a number between 0 and 9"
  "A game where one player chooses a number and the other player guess it. For two players.")
