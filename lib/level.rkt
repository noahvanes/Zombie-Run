;LIB/LEVEL.RKT
;=============

#lang r5rs

(#%require "OO-syntactic-sugar.rkt")
(#%require "coordinates.rkt")
(#%require "screen-objects.rkt")
(#%require (only "graphics-engine.rkt" grey black))
(#%require (only racket/base when unless random))
(#%provide level)

;************************************************************
;* LEVEL                                                    *
;* - Contains ADT level to randomize the game-environment   *
;* - Automatically loads new levels to make game infinite   *
;*                                                          *
;*                                        - By Noah Van Es  *
;************************************************************

;random function
(define (random-int a b)
  (+ a (random (- b (- a 1)))))

;random only works with integers
(define (int x) 
  (inexact->exact (round x)))

;;make lineair (first order) function
(define (make-lineair-function x1 x2 y1 y2)
  (lambda (x) (+ (* (- x x1) (/ (- y2 y1) 
                                (- x2 x1)))
                 y1)))

; LEVEL
; -----
(define (level difficulty width height max-lvl seconds/jump floor-height background object-color)
  (let* ((scale-between (lambda (min max) (int ((make-lineair-function 1 max-lvl min max) difficulty))))
         (object-amount (scale-between 10 30))
         (number-of-objects (if (= difficulty max-lvl) -1 object-amount))
         (globalspeed (scale-between -200 -500))
         (min-height (scale-between 80 (- height 350)))
         (max-height (scale-between 200 (- height 150)))
         (min-wait (scale-between 80 50))
         (max-wait (scale-between 150 115))
         (allowed-buffer (scale-between 150 50))
         (max-jump (- (+ max-height allowed-buffer) floor-height))
         (timeout (random-int min-wait max-wait))
         (current timeout))
    
    (define (get-background-image) background)
    (define (get-difficulty) difficulty)
    
    (define (spawn-obstacle)
      (let ((o-width (random-int 80 130)) 
            (o-height (random-int min-height max-height)))
        (obstacle o-width o-height width globalspeed object-color)))
    
    (define (spawn-zombie)
      (zombie 50 87 width globalspeed))
    
    (define (spawn-ammo-bonus)
      (let ((amount (random-int 1 3))
            (py (random-int 0 max-height)))
        (ammo-bonus 50 50 width py globalspeed amount)))
    
    (define (spawn-score-bonus)
      (let ((amount (random-int 500 1500))
            (py (random-int 0 max-height)))
        (score-bonus 50 50 width py globalspeed amount)))
    
    (define (set-physics! avatar engine)
      (let* ((avatar-vy (/ (* 2 max-jump) seconds/jump))
             (gravity (/ avatar-vy seconds/jump)))
        (ask avatar set-vy! avatar-vy)
        (ask engine set-physics-values! gravity floor-height)))
    
    (define (load-next-level gameloop)
      (let ((new-difficulty (+ difficulty 1)))
        (ask gameloop load-level 
             (level new-difficulty width height max-lvl seconds/jump 
                    floor-height background object-color))))
    
    (define (spawn-random)
      (let ((choice (random-int 1 10)))
        (cond ((<= choice 1) (spawn-ammo-bonus))
              ((<= choice 2) (spawn-score-bonus))
              ((<= choice 6) (spawn-obstacle))
              ((<= choice 10) (spawn-zombie)))))
    
    (define (update gameloop)
      (cond ((< current timeout) (set! current (+ current 1)))
            ((= number-of-objects 0) (load-next-level gameloop))
            (else (ask gameloop add-object! (spawn-random))
                  (set! number-of-objects (- number-of-objects 1))
                  (set! timeout (random-int min-wait max-wait))
                  (set! current 0))))
    
    (dispatcher update
                set-physics!
                get-background-image
                get-difficulty)))