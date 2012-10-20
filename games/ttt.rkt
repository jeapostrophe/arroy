#lang racket/base
(require "../lts.rkt"
         racket/match
         racket/port
         racket/unit)

;; Tic-Tac-Toe is an example of an LTS
(define-unit game@
  (import)
  (export lts^)

  ;; The game is either in the middle or at the end
  (struct ttt () #:transparent)
  (struct middle ttt (mover board) #:transparent)
  (struct end ttt (winner) #:transparent)

  ;; A smart constructor that detects when the game is over. Could be
  ;; more efficient.
  (define (middle* p b)
    (define last (swap p))
    (define (set-members? s l)
      (for/and ([e (in-list l)])
        (equal? (hash-ref s e #f) last)))
    ;; XXX Could be more efficient
    (define last-won?
      (or (for/or ([x (in-range 3)])
            (set-members? b
                          (list (cons x 0)
                                (cons x 1)
                                (cons x 2))))
          (for/or ([y (in-range 3)])
            (set-members? b
                          (list (cons 0 y)
                                (cons 1 y)
                                (cons 2 y))))
          (set-members? b
                        (list (cons 0 0)
                              (cons 1 1)
                              (cons 2 2)))
          (set-members? b
                        (list (cons 0 2)
                              (cons 1 1)
                              (cons 2 0)))))
    (define board-full?
      (for*/and ([x (in-range 3)]
                 [y (in-range 3)])
        (hash-has-key? b (cons x y))))
    (cond
      [board-full?
       (end #f)]
      [last-won?
       (end last)]
      [else
       (middle p b)]))

  (define (make-initial-state players)
    (and (= 2 players)
         (middle (random players) (hash))))

  ;; There's only one kind of a move, placing your mark.
  (struct move () #:transparent)
  (struct place move (x y) #:transparent)

  (define moves
    (for*/list ([x (in-range 3)]
                [y (in-range 3)])
      (place x y)))

  ;; XXX Make this a utility or a unit that takes "moves" and produces
  ;; "available"
  (define (available s p)
    (filter (λ (m) (next s p m)) moves))

  (define (score s p)
    (match* (s p)
      [((end p) p) 1]
      [(_ _) 0]))

  (define (render s p)
    (match s
      [(place x y)
       (format "(~a,~a)" x y)]
      [(middle _ b)
       (with-output-to-string
         (λ ()
           (newline)
           (for ([x (in-range 3)])
             (for ([y (in-range 3)])
               (display
                (match (hash-ref b (cons x y) #f)
                  [ 0 "O"]
                  [ 1 "X"]
                  [#f " "])))
             (newline))))]
      [(end wp)
       (format "Player ~a won!" wp)]))

  (define swap
    (match-lambda
     [0 1]
     [1 0]))

  ;; You can only place in the middle of the game and only if the
  ;; square is not occupied.
  (define (next s p m)
    (match s
      [(middle (== p) b)
       (match-define (place x y) m)
       (and (not (hash-has-key? b (cons x y)))
            (middle* (swap p) (hash-set b (cons x y) p)))]
      [_
       #f])))

(provide game@)
