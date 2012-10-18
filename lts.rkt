#lang racket/base
(require racket/unit
         racket/contract)

;; XXX Should this be Xexpr? Sprite list? (U Board Menu)?
(define display-rep?
  string?)

(define player?
  exact-nonnegative-integer?)

;; A labelled transition system is a...
;;  make-initial-state : number -> (U #f state)
;;  available          : state player -> (listof moves)
;;  next               : state player move -> Maybe state
;;  score              : state player -> number
;;  render             : (U state move) player -> display-rep
(define-signature lts^
  (make-initial-state available next score render))

;; XXX There should be a way to have a "computer" player that
;; represents action of the game, like rolling dice, and it should
;; have integrated clock (measured in abstract time) to control when
;; it should happen
;;
;; It turns out this is more complicated than I thought, because I
;; originally wanted to have player? include a (struct computer
;; (clock)) type, but then the arroy@ unit would have to query for the
;; possible moves of EVERY possible value for clock. Clearly
;; unworkable. If you always had clock = 0, then you could have it
;; always query for the single computer player. But how could you
;; integrate in other clock values? If the only valid clock was "1",
;; then you could do that, but that might be gross... although you
;; could simulate all other clock values in the game model. (I'm
;; thinking of how you'd do the Active Time Battle in FF, for
;; example.)

(provide 
 lts^
 (contract-out
  [display-rep?
   (-> any/c boolean?)]
  [player?
   (-> any/c boolean?)]))
