#lang racket/base
(require "lts.rkt"
         racket/function
         racket/list
         racket/match
         racket/unit)

;; XXX In the Web version, have a login and then list "games in play"
;; and you can join or start a new one. When you start one, give it a
;; number/code and the number of players and a user-defined, it starts
;; when all the players join.
;; 
;; The server serializes the state (add the fact that the state must
;; be serializable to the contract on lts^) to disk after every
;; transition. When users go to the site, it informs them of the
;; result of render on the current state and their available moves.
;;
;; When the computer user is supported, the server should probably
;; start a thread for every active game that communicates via channels
;; with the Web threads that represent the players and the server
;; thread could run the computer actions. This would also make it easy
;; to do COMET-style pushing to the clients of state changes.
;;
;; It would be neat to have the state for a game and its sequence of
;; moves recorded on the server, so that other people could view old
;; games. (Given that everything would be serializable, it would be
;; easy to do.)
;;
;; I think this setup will complicate using a unit because the server
;; would have to have some data-structure that stores all the
;; available game types, but if it stored the units, then they'd be
;; invoked many times without purpose. I think a better way would be
;; to swap out the unit for a single structure that encapsulated all
;; the pieces (basically an object, although I find the Racket object
;; system kind-of annoying). This would also make the contracts
;; enforceable [technically Racket signatures can have contracts, but
;; they can't refer to each other, so you couldn't have the signature
;; contain a state? and move? predicate.] If we used a struct, then it
;; would also be easy to have a "name" associated with each game type.

(define (sum l)
  (foldl + 0 l))
(define (average l)
  (if (empty? l)
    0
    (/ (sum l) (length l))))

;; play : number -> (U #f play-the-game)
(define-signature arroy^
  (play))

;; Arroy takes an LTS and presents a stepper for it, but it also
;; annotates that options with whether the current player can win.
(define-unit arroy@
  (import lts^)
  (export arroy^)

  (define (play players)
    (define initial-state
      (make-initial-state players))

    ;; XXX Memoization of these two functions would be good for
    ;; performance. Also, the memoization on the second one should
    ;; have the interesting rule that
    ;;
    ;; (gscore s p i)
    ;;
    ;; can be used for
    ;;
    ;; (gscore s p j)
    ;;
    ;; provided j <= i, because you can always use more precision than
    ;; you asked for, but you don't want less.
    (define (available* s)
      (apply append
             (for/list ([p (in-range players)])
               (map (curry cons p)
                    (available s p)))))
    (define (gscore s p depth)
      (cond
        [(zero? depth)
         (score s p)]
        [else
         (define p*ms (available* s))
         (cond
           [(empty? p*ms)
            (score s p)]
           [else
            (average
             (filter-map
              (λ (p*m)
                (define mp (car p*m))
                (define m (cdr p*m))
                (define ns (next s mp m))
                (and ns (gscore ns p (sub1 depth))))
              p*ms))])]))

    (and
     initial-state
     (let loop ([s initial-state])
       (match
           ;; XXX In an async version, we should allow every place to
           ;; submit its move rather than just waiting for the first
           ;; one. That is:
           ;;
           ;; (send all-players s)
           ;; (apply sync (map handle-player-action all-player-channels))
           ;; loop with this next state
           (for/or ([p (in-range players)])
             (match (available s p)
               [(list) #f]
               [a
                (printf "Player ~a's turn\n"
                        p)
                (printf "Current state: ~a\n"
                        (render s p))
                (printf "Available moves:\n")
                (for ([m (in-list a)]
                      [i (in-naturals)])
                  (printf "\t~a. ~a (~a)\n"
                          i
                          (render m p)
                          (real->decimal-string
                           ;; XXX Change this to a 9 for TTT
                           ;; XXX It can be 3 for Guess
                           (gscore (next s p m) p 0))))
                (let read-loop ([i +inf.0])
                  (cond
                    [(and (exact-nonnegative-integer? i)
                          (< i (length a)))
                     (next s p (list-ref a i))]
                    [else
                     (printf "Selection: ")
                     (flush-output)
                     (read-loop (read))]))]))
         [#f
          (printf "Results\n")
          (for ([p (in-range players)])
            (printf "\t~a. ~a\n"
                    p (score s p)))]
         [ns
          (loop ns)])))))

(provide arroy@
         arroy^)
