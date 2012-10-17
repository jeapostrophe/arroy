#lang racket/base
(require racket/list
         racket/unit)

;; A labelled transition system is a...
;;  initial-state : state
;;  moves         : (listof moves)
;;  next          : state move -> Maybe state
;;  winner        : state -> player
;;  player        : state -> player
(define-signature lts^
  (initial-state moves next winner player))

(provide lts^)

;; XXX I think it would be better to have
;;  initial-state : -> state
;; for games with random initial states (like a card game)
;; Or just have them start with a big random move selection

;; XXX I think it would be better to have a
;;  players : (listof player)
;; export, to specify how many players there are

;; XXX Something even better would be to have players always numbered
;; (0...n) and then have the initial-state function be:
;;  initial-state : number -> (U #f state)
;; where it returns false if the game cannot be played with that many players

;; XXX Change to
;;  available : state player -> (listof moves)
;; to handle games where players can act "simultaneously" before the game necessarily advances
;; (Ending would be when all the players are done)

;; XXX Add
;;  display : (U move state) player -> display-rep
;; to view the representation of the state visible to a player. What
;; is a good representation? String? Xexpr? Sprite list? It is not clear.

;; XXX Add a distinguished player (computer clock) that will randomly
;; choose an available move after clock time units. If clock is 0,
;; then this can represent dice rolls. If clock is positive, then it
;; can represent games with time limits, automatic actions, etc. The
;; meaning of the unit could be agreed upon by players before
;; starting--maybe a day, maybe minutes, maybe frames.

;; XXX Change from winner to
;;  score : state player -> number
;; for games without distinct winners and better AIs

;; XXX Sometimes the challenge of a game is knowing which moves are
;; valid (like GHOST), in these cases you want to display invalid
;; moves to the user. If such a move is chosen, then you may want the
;; game to automatically convert it into an "error" move. You could do
;; this totally within the model (by have available return "bad"
;; moves), but it may be useful to incorporate that into the
;; interface. (It isn't necessary though.)