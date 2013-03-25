;LIB/QUEUE.RKT
;=============

#lang r5rs

(#%require "OO-syntactic-sugar.rkt")
(#%require (only racket/base when unless))
(#%provide queue)

;*****************************************************************
;* QUEUE                                                         *
;* - A standard queue with optimalization to keep things in O(1) *
;*                                                               *
;*                                           - By Noah Van Es    *
;*****************************************************************

; QUEUE
; -----
(define (queue . elements)
  (let ((contents '())
        (last #f)
        (length 0))
    
    (define (initialize)
      (for-each (lambda (el)
                  (enqueue! el))
                elements))
    
    (define (enqueue! el)
      (if last
          (let ((cell (cons el '())))
            (set-cdr! last cell)
            (set! last cell))
          (let ((cell (cons el '())))
            (set! contents cell)
            (set! last cell)))
      (set! length (+ length 1)))
    
    (define (first)
      (when last
        (car contents)))
    
    (define (serve!)
      (unless (null? contents)
        (let ((first (car contents)))
          (set! length (- length 1))
          (cond ((eq? contents last) (set! contents (cdr contents))
                                     (set! last #f)
                                     first)
                (else (set! contents (cdr contents))
                      first)))))
    
    (define (empty?) (not last))
    (define (get-contents) contents)
    (define (get-length) length)
    
    (initialize)
    
    (dispatcher enqueue!
                serve!
                get-contents
                get-length
                empty?
                first)))