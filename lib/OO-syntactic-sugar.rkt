;LIB/OO-SYNTACTIC-SUGAR.RKT
;==========================

#lang r5rs

(#%require (only racket/base error))
(#%provide ask dispatcher)

;**********************************************************
;* OO-SYNTACTIC-SUGAR                                     *
;* - small syntax definition for object message passing   *
;* - easier syntax for dispatching to internal procedures *
;*                                                        *
;*                                     - By Noah Van Es   *
;**********************************************************

(define-syntax ask
  (syntax-rules ()
    ((ask object command) (object 'command))
    ((ask object command p1 ...) (object 'command p1 ...))))

(define-syntax dispatcher
  (syntax-rules ()
    ((dispatcher m1 ...)
     (lambda (msg . pm)
       (cond ((eq? msg 'm1) (apply m1 pm))
             ...
             (else (error "unknown message: ~a" msg)))))))