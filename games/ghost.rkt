#lang racket/base
(require "../lts.rkt"
         racket/file
         racket/list
         racket/match
         racket/set)

(define dict
  (for/fold ([dict (set)])
      ([word (in-list
              (filter
               (λ (s) (not
                       (or (regexp-match "'" s) ; no apostrophes
                           (regexp-match #rx"[A-Z]" s)))) ; no proper nouns
               (file->lines #:mode 'text "/usr/share/dict/words")))])
    (set-add dict word)))

(define (letters->string letters)
  (list->string (reverse letters)))

(define (forms-word? letters)
  (set-member? dict (string-downcase (letters->string letters))))

(define (next-turn i n)
  (remainder (add1 i) n))

(define (last-turn i n)
  (remainder (+ (sub1 i) n) n))

(struct state ())
(struct middle state (letters turn n))
(struct challenged state (letters challenger challenged))
(struct defending state (letters challenger challenged))
(struct end state (loser))

(struct move ())
(struct challenge move ())
(struct declare move ())
(struct forfeit move ())
(struct add move (letter))

(define add-letters
  (for/list ([c (in-list (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))])
    (add c)))

(define (make-initial-state n)
  (and (> n 1) (middle empty 0 n)))

(define available
  (match-lambda*
   [(list (middle letters p _) p)
    (let* ([moves add-letters]
           [moves
            (if (> (length letters) 0)
              (cons (challenge) moves)
              moves)]
           [moves
            (if (> (length letters) 2)
              (cons (declare) moves)
              moves)])
      moves)]
   [(list (challenged _ _ p) p)
    (cons (forfeit) add-letters)]
   [(list (defending _ _ p) p)
    (list* (declare) (forfeit) add-letters)]
   [(list _ _)
    empty]))

(define next
  (match-lambda*
   ;; If there are letters, challenge.
   [(list (middle letters p n) p (challenge))
    (and (> (length letters) 0) (challenged letters p (last-turn p n)))]
   ;; If there are more than two letters, declare.
   [(list (middle letters p n) p (declare))
    (and (> (length letters) 2) (if (forms-word? letters)
                                  (end (last-turn p n))
                                  (end p)))]
   ;; If adding letters, add a letter.
   [(list (middle letters p n) p (add c))
    (middle (cons c letters) (next-turn p n) n)]
   ;; Challenged person begins to finish the word
   [(list (challenged letters er p) p (add c))
    (defending (cons c letters) er p)]
   ;; challenged [person] forfeits
   [(list (challenged letters er p) p (forfeit))
    (end p)]
   ;; defender adds letter
   [(list (defending letters er p) p (add c))
    (defending (cons c letters) er p)]
   ;; defender forfeits
   [(list (defending letters er p) p (forfeit))
    (end p)]
   ;; defender finishes word
   [(list (defending letters er p) p (declare))
    (if (forms-word? letters)
      (end er)
      (end p))]
   ;; Or you can't do anything
   [_
    #f]))

(define score
  (match-lambda*
   [(list (end p) p)
    -1]
   [(list _ _)
    0]))

(define render
  (match-lambda*
   [(list (middle letters _ _) _)
    (if (> (length letters) 0)
      (letters->string letters)
      "No letters have been added yet.")]
   [(list (challenged letters er ee) er)
    (format "You challenged player ~a on ~a."
            ee (letters->string letters))]
   [(list (challenged letters er ee) ee)
    (format "You were challenged by player ~a on ~a."
            er (letters->string letters))]
   [(list (challenged letters er ee) _)
    (format "Player ~a challenged player ~a on ~a."
            er ee (letters->string letters))]
   [(list (defending letters er ee) ee)
    (letters->string letters)]
   [(list (defending letters _ ee) _)
    (format "Player ~a is attempting to finish ~a."
            ee (letters->string letters))]
   [(list (end p) p)
    "You were eliminated."]
   [(list (end p) _)
    (format "Player ~a was eliminated." p)]
   [(list (add c) _)
    (format "add ~a" c)]
   [(list (challenge) _)
    "challenge"]
   [(list (declare) _)
    "declare current state to be a word"]
   [(list (forfeit) _)
    "give up"]
   [(list _ _)
    "<RENDER NOT IMPLEMENTED>"]))

(define-lts
  "GHOST"
  "A game where players alternate adding letters to a word, trying to be the last to complete a word. For any number of players.")
