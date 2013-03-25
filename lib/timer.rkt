;LIB/TIMER.RKT
;=============

#lang racket

(#%require "OO-syntactic-sugar.rkt")

(provide current-time
         milli-timer
         timer)

;********************************************************** 
;* TIMER                                                  *
;* - Library with a timer to check current time           * 
;* - Also contains a small timer to measure milliseconds  *
;*                                                        *
;*                    - By SOFT [Software Languages Lab]  * 
;*                    - Small adjustments by Noah Van Es  *
;**********************************************************

; TIMER
; -----
(define CPU_FREQ 60000000)

(define (timer)
  (define time 0) 
  (define wait-time (/ 1 CPU_FREQ))
  
  (define timer-thread
    (thread (lambda ()
              (let loop ()
                (sleep wait-time)
                (set! time (+ 1 time))
                (loop)))))
  
  (define (dispatch msg . pm)
    (cond ((eq? msg 'time) time)
          ((eq? msg 'start) (thread-resume timer-thread))
          ((eq? msg 'restart)(set! time 0)
                             (thread-resume timer-thread))
          ((eq? msg 'reset) (thread-suspend timer-thread)
                            (set! time 0))
          ((eq? msg 'stop) (thread-suspend timer-thread))
          ((eq? msg 'set-period!) 
           (apply (lambda (period)
                (set! wait-time (* (+ period 1) 
                                   (/ 1 CPU_FREQ)))) 
                  pm))))
  (thread-suspend timer-thread)
  (set! time 0)
  dispatch)

(define timer0 (timer))
(ask timer0 set-period! 100000)
(define (stop-timer timer) (ask timer stop))
(define (start-timer timer) (ask timer start))
(define (restart-timer timer) (ask timer restart))
(define (reset-timer timer) (ask timer reset))
(define (read-timer timer)  (ask timer time))
(define (write-timer-period timer period) 
  (ask timer set-period! period))

; MILLI-TIMER
; -----------
(define (milli-timer)
  (let ((start-time (current-inexact-milliseconds)))
    
    (define (reset!)
      (set! start-time (current-inexact-milliseconds)))
    
    (define (time-elapsed)
      (- (current-inexact-milliseconds) start-time))
    
    (dispatcher reset!
                time-elapsed)))

; CURRENT-TIME
; ------------
(start-timer timer0)

(define (current-time-old)
  (ask timer0 time))

(define GLOBALTIMER (milli-timer))
(define (current-time)
  (/ (ask GLOBALTIMER time-elapsed) 1000))