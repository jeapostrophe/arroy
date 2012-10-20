#lang racket/base
(require "lts.rkt"
         racket/function
         racket/list
         racket/match
         racket/runtime-path
         web-server/servlet-env
         web-server/dispatch
         web-server/servlet)

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

(define (sum l)
  (foldl + 0 l))
(define (average l)
  (if (empty? l)
    0
    (/ (sum l) (length l))))

;; Arroy takes an LTS and presents a stepper for it, but it also
;; annotates that options with whether the current player can win.
;; play : lts number -> (U #f play-the-game)
(define (play an-lts players)
  (match-define
   (lts name description make-initial-state available next score render)
   an-lts)

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
        (loop ns)]))))

(define-runtime-path static-dir "static")
(define (go port games)
  (define-values
    (main-dispatch main-url)
    (dispatch-rules
     [()
      page/main]
     [("")
      page/main]
     [("game" (integer-arg))
      page/game]
     [("game" (integer-arg) (integer-arg))
      page/game/player]
     [("game" "new")
      page/game/new]))

  (define (template title . body)
    (response/xexpr
     `(html
       (head
        (title ,title))
       (body
        (h1 ,title)
        ,@body))))

  (struct game-in-progress (game players state))
  (define in-progress
    (make-hasheq))

  (define (page/main req)
    (template
     "Main"
     `(div ([class "games"])
           (ul
            ,@(for/list ([(g s) (in-hash in-progress)])
                `(li (a ([href ,(main-url page/game g)])
                        ,(format "~a" g))))))
     `(a ([href ,(main-url page/game/new)])
         "New Game")))

  (define (page/game req game-id)
    (match-define
     (game-in-progress game players state)
     (hash-ref in-progress game-id))
    (template
     (format "Game > ~a" game-id)
     "Which player are you?"
     `(ul
       ,@(for/list ([p (in-range players)])
           `(li (a ([href ,(main-url page/game/player game-id p)])
                   ,(format "Player ~a" p)))))))

  (define (page/game/player req game-id player-id)
    (match-define
     (game-in-progress game players state)
     (hash-ref in-progress game-id))
    (match-define
     (lts _ _ _ available next score render)
     game)
    (define chosen-m
      (send/suspend/dispatch
       (λ (embed/url)
         (template
          (format "Game > ~a > ~a" game-id player-id)

          `(h1 "Current state:")
          (render state player-id)
          `(h1 "Available moves:")
          `(ul
            ,@(for/list ([m (in-list (available state player-id))])
                `(li
                  (a ([href ,(embed/url (λ (req) m))])
                     ,(render m player-id)))))
          `(h1 "Results")
          `(ul
            ,@(for/list ([p (in-range players)])
                `(li ,(format "~a. ~a"
                              p (score state p)))))))))
    (define next-state
      (next state player-id chosen-m))
    (when next-state
      (hash-set! in-progress
                 game-id
                 (game-in-progress
                  game
                  players
                  next-state)))
    (redirect-to
     (main-url page/game/player game-id player-id)))

  (define (page/game/new req)
    (define which-game
      (send/suspend/dispatch
       (λ (embed/url)
         (template
          "Game > New"
          "What kind of game?"
          `(ul
            ,@(for/list ([g (in-list games)])
                `(li (p (a ([href ,(embed/url
                                    (λ (req) g))])
                           ,(lts-name g)))
                     (p ,(lts-description g)))))))))
    ;; XXX Ask the user about this
    (define how-many-players 2)
    (define initial-state
      ((lts-make-initial-state which-game)
       how-many-players))
    (define this-game
      (game-in-progress
       which-game
       how-many-players
       initial-state))

    (define game-id
      (hash-count in-progress))
    (hash-set! in-progress
               game-id
               this-game)
    (redirect-to
     (main-url page/game game-id)))

  (serve/servlet
   main-dispatch
   #:extra-files-paths (list static-dir)
   #:command-line? #t
   #:servlet-regexp #rx""
   #:listen-ip #f
   #:port port))

(module+ main
  (require (for-syntax racket/base))
  (begin-for-syntax
    (require racket/runtime-path)
    (define-runtime-path games-dir "games"))
  (define-syntax (define-games stx)
    (syntax-case stx ()
      [(_ games-id)
       (with-syntax
           ([(game ...)
             (for/list ([game-path (in-list (directory-list games-dir))]
                        #:when (file-exists?
                                (build-path games-dir game-path)))
               (quasisyntax/loc stx
                 (let ()
                   (local-require
                    (file #,(path->string
                             (build-path games-dir game-path))))
                   game)))])
         (syntax/loc stx
           (define games-id
             (list game ...))))]))

  (define-games games)

  (go 8080 games))
