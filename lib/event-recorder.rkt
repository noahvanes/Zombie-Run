;LIB/EVENT-RECORDER.RKT
;======================

#lang r5rs

(#%require (only racket/base error))
(#%require (only graphics/graphics query-mouse-posn ready-mouse-click mouse-click-posn posn-x posn-y))
(#%require "coordinates.rkt")
(#%require "OO-syntactic-sugar.rkt")
(#%provide event-recorder)

;*********************************************************
;* EVENT RECORDER                                        *
;* - processes the user's input                          *
;* - support for combo's and keypress-releases           *
;* - mouse support (query position and clicks)           *
;*                                                       *
;*                                     - By Noah Van Es  *
;*********************************************************

;EVENT-RECORDER
;--------------
(define (event-recorder viewport width height)
  (let ((event1 #f)    
        (event2 #f)
        (event3 #f))
    
    (define (register-key-event! event)
      (set! event3 event2)
      (set! event2 event1)
      (set! event1 event))
    
    (define (clear)
      (set! event1 #f)
      (set! event2 #f)
      (set! event3 #f))
    
    (define (last-recorded-event) event1)
    (define (last-events) (vector event1 event2 event3))
    
    (define (get-mouse-position) 
      (let ((current-posn (query-mouse-posn viewport)))
        (if current-posn
            (coordinates (posn-x current-posn)
                         (- height (posn-y current-posn)))
            (coordinates -1 -1)))) ;when off-screen
    
    (define (get-last-click)
      (let* ((last (ready-mouse-click viewport)))
        (if last
            (let ((position (mouse-click-posn last)))
              (coordinates (posn-x position)
                           (- height (posn-y position))))
            (coordinates -1 -1))))

      (dispatcher clear 
                  register-key-event!
                  last-recorded-event
                  last-events
                  get-mouse-position
                  get-last-click)))