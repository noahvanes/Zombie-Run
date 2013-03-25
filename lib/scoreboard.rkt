;LIB/SCOREBOARD.RKT
;==================

#lang r5rs

(#%require (only racket/base when unless))
(#%require "OO-syntactic-sugar.rkt")
(#%require "data-file.rkt")
(#%provide scoreboard)

;**************************************************************
;* SCOREBOARD                                                 *
;* - A simple scoreboard to keep track of the player's score  *
;* - PLANNED -- DONE: highscore support                       *
;*                                                            *
;*                                          - By Noah Van Es  *
;**************************************************************

; MINI-ADT's FRAME & RECORD 
;(functional style is necessary to work with external files)
; ----------------------------------------------------------

;;frame
(define (frame name score)
  (cons name score))

(define (name frame)
  (car frame))

(define (score frame)
  (cdr frame))

;;record
(define the-empty-record (list))
(define (empty-record? rec) 
  (eq? rec the-empty-record))
(define (record? obj)
  (list? obj))

(define (add frame rec)
  (cons frame rec))

(define (first record)
  (car record))

(define (rest record)
  (cdr record))

(define (insert frame record)
  (cond ((empty-record? record) (add frame the-empty-record))
        ((< (score (first record)) (score frame)) (add frame record))
        (else (add (first record) (insert frame (rest record))))))

(define (first-n record n)
  (if (or (= n 0) (empty-record? record))
      the-empty-record
      (add (first record)
           (first-n (rest record) (- n 1)))))

; SCOREBOARD
; ----------
(define (scoreboard initial-score initial-multiplier highscores-file)
  (let ((current-score initial-score)
        (multiplier initial-multiplier))
    
    (define (initialize)
      (when (or (ask highscores-file empty?) 
                (not (record? (ask highscores-file get-content))))
        (ask highscores-file set-content! the-empty-record)))
    
    (define (get-score) current-score)
    (define (get-multiplier) multiplier)
    (define (set-multiplier! m)
      (set! multiplier m))
    
    (define (get-highscores) 
      (ask highscores-file get-content))
    
    (define (get-highscore)
      (let ((highscores (get-highscores)))
        (unless (empty-record? highscores)
          (score (first highscores)))))
    
    (define (insert-highscore! name score)
      (define (highscore-updater highscores)
        (let ((new-frame (frame name score)))
          (insert new-frame highscores)))
      (ask highscores-file update-content! highscore-updater))
    
    (define (cut-highscores! amount)
      (define highscore-updater (lambda (highscores) (first-n highscores amount)))
      (ask highscores-file update-content! highscore-updater))
    
    (define (highscores->strings amount max-length)
      (let ((highscores (get-highscores)))
        (define (make-whitespace x) (make-string x #\space))
        (define (frame->string position frame)
          (let* ((player-name (name frame))
                 (name-length (string-length player-name))
                 (player-score (number->string (score frame)))
                 (score-length (string-length player-score)))
            (string-append 
             (make-whitespace 1)
             (number->string position) 
             "."
             (make-whitespace 1)
             player-name
             (make-whitespace (- max-length 3 name-length score-length))
             player-score))) 
        (let loop ((current highscores) (position 1))
          (if (or (> position amount) (empty-record? current))
              (list)
              (cons (frame->string position (first current))
                    (loop (rest current) (+ position 1)))))))
    
    
    (define (clear-highscores!)
      (ask highscores-file set-content! the-empty-record))
    
    (define (update-score)
      (set! current-score (+ current-score multiplier)))
    
    (define (incr! v)
      (set! current-score (+ current-score v)))
    
    (define (reset!)
      (set! current-score initial-score)
      (set! multiplier initial-multiplier))
    
    (initialize)
    
    (dispatcher get-score
                update-score
                get-multiplier
                set-multiplier!
                incr!
                reset!
                get-highscores
                get-highscore
                insert-highscore!
                cut-highscores!
                clear-highscores!
                highscores->strings)))