;LIB/OPTION.RKT
;==============

#lang r5rs

(#%require (only racket/base error))
(#%require "OO-syntactic-sugar.rkt")

(#%provide option-box)

;************************************************************
;* OPTION                                                   *
;* - Clickable menu item with custom text and color         *
;*                                                          *
;*                                      - By Noah Van Es    *
;************************************************************

; OPTION
; ------
(define (option-box text action position width height hover-color)
  
  (define (get-text) text)
  (define (get-position) position)
  (define (get-width) width)
  (define (get-height) height)
  (define (get-hover-color) hover-color)
  
  (define (activate)
    (action))
  
  ;internal procedure to check a 'hit' with a given position
  (define (hover? co)
    (if co 
        (let ((px (ask co get-x))
              (py (ask co get-y)))
          (and (< py (+ (ask position get-y) height))
               (< px (+ (ask position get-x) width))
               (> py (ask position get-y))
               (> px (ask position get-x))))))
  
    (dispatcher get-text
                get-position
                get-width
                get-height
                get-hover-color
                hover?
                activate))