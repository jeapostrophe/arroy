#lang racket/base
(require racket/contract)

;; XXX Should this be Xexpr? Sprite list? (U Board Menu)?
(define display-rep?
  string?)

(define player?
  exact-nonnegative-integer?)

(struct lts
        (name
         description
         make-initial-state
         available
         next
         score
         render))

(define (make-lts
         #:name name
         #:description description
         #:state? state?
         #:move? move?
         #:make-initial-state make-initial-state
         #:available available
         #:next next
         #:score score
         #:render render)
  (lts name description make-initial-state available next score render))

(require (for-syntax racket/base
                     syntax/strip-context))
(define-syntax (define-lts stx)
  (syntax-case stx ()
    [(_ name description)
     (replace-context
      stx
      (syntax/loc stx
        (begin
          (define game
            (make-lts
             #:name
             name
             #:description
             description
             #:state?
             state?
             #:move?
             move?
             #:make-initial-state
             make-initial-state
             #:available
             available
             #:next
             next
             #:score
             score
             #:render
             render))

          (provide game))))]))

(provide
 define-lts
 lts
 (contract-out
  [display-rep?
   (-> any/c boolean?)]
  [player?
   (-> any/c boolean?)]
  [lts?
   (-> any/c boolean?)]
  [make-lts
   (->d
    (#:name
     [name
      string?]
     #:description
     [description
      string?
      ]#:state?
       [state?
        (-> any/c boolean?)]
       #:move?
       [move?
        (-> any/c boolean?)]
       #:make-initial-state
       [make-initial-state
        (-> exact-nonnegative-integer?
            (or/c #f state?))]
       #:available
       [available
        (-> state? player?
            (listof move?))]
       #:next
       [next
        (-> state? player? move?
            (or/c false/c state?))]
       #:score
       [score
        (-> state? player?
            number?)]
       #:render
       [render
        (-> (or/c state? move?) player?
            display-rep?)])
    [the-lts lts?])]))


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
