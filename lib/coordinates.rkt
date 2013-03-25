;LIB/COORDINATES.RKT
;===================

#lang r5rs

(#%require (only racket/base error))
(#%require "OO-syntactic-sugar.rkt")
(#%provide coordinates)

;***************************************************
;* COORDINATES                                     *
;* - A cartesian pair that holds an x and y value  *
;* - Can also be used to manipulate speed etc.     *
;*                                                 *
;*                               - By Noah Van Es  *
;***************************************************

(define (coordinates x y)
  
  (define (get-x) x)
  (define (get-y) y)
  
  (define (set-x! v) 
    (set! x v))
  (define (set-y! v)
    (set! y v))
  
  (define (op-x! op v)
    (set-x! (op x v)))
  (define (op-y! op v)
    (set-y! (op y v)))

  (dispatcher get-x
              get-y
              set-x!
              set-y!
              op-x!
              op-y!))