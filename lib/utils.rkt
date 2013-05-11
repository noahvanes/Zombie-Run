;LIB/UTILS.RKT
;=============

#lang r5rs 

;**********************************************************
;* UTILS                                                  *
;* - contains some general-purpose/useful procedures      *
;* - contains syntax definitions for when & unless        *
;* - contains some syntax/procedures for OO-dispatching   *
;*                                                        *
;*                                     - By Noah Van Es   *
;**********************************************************

;;some useful procedures
(define (int x) (inexact->exact (round x)))
(define (double x) (+ x x))
(define (square x) (* x x))

;;some constants
(define pi (acos -1))

;;when & unless
(define-syntax when
  (syntax-rules ()
    ((when c b1 ...) 
     (if c 
         (begin b1 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((when c b1 ...) 
     (if (not c) 
         (begin b1 ...)))))

;;OO-syntactic-sugar + error-procedure
(define-syntax ask
  (syntax-rules ()
    ((ask object command) (object 'command))
    ((ask object command p1 ...) (object 'command p1 ...))))

(define (message-error obj msg)
  (string-append (symbol->string obj)
                 "> unknown message: "
                 (symbol->string msg)))