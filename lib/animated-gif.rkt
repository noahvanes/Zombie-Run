;LIB/ANIMATED-GIF.RKT
;====================

#lang r5rs

(#%require "OO-syntactic-sugar.rkt")
(#%require (only racket/base unless))
(#%provide animated-gif)

;****************************************************
;* ANIMATED-GIF                                     *
;* - emulates a gif animation with a circular list  *
;*                                                  *         
;*                             - By Noah Van Es     *
;****************************************************

;;last procedure
(define (last lst)
  (if (null? (cdr lst))
      lst
      (last (cdr lst))))

(define (make-circular! lst)
  (set-cdr! (last lst) lst))

; ANIMATED-GIF
; ------------
(define (animated-gif image-sequence timeout)
  (let ((delay 0)
        (current image-sequence))
    
    (define (initialize)
      (make-circular! current))
    
    (define (next-image!)
      (set! delay (+ delay 1))
      (if (> delay timeout)
          (let ((first (car current)))
            (set! current (cdr current))
            (set! delay 0)
            first)
          (car current)))
    
    (define (reset-animation!)
      (set! current image-sequence)
      (set! delay 0))
    
    (initialize)
    
    (dispatcher next-image! 
                reset-animation!)))