#lang racket/base
(require "lts.rkt"
         racket/function
         racket/list
         racket/match
         racket/unit)

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
                           (gscore (next s p m) p 4))))
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
