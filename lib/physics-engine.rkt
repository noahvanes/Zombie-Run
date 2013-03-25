;LIB/PHYSICS-ENGINE.RKT
;======================

#lang r5rs 

(#%require (only racket/base error unless when)) 
(#%require "timer.rkt")
(#%require "coordinates.rkt")
(#%require "OO-syntactic-sugar.rkt")

(#%provide (all-defined))

;*****************************************************
;* PHYSICS ENGINE                                    *
;* - takes care of gravity and other dynamic phyics  *
;* - calculates new positions and speeds             *
;*                                                   *
;*                                 - By Noah Van Es  *
;*****************************************************

(define (speed-zero)
  (coordinates 0 0))

(define (avg a b)
  (/ (+ a b) 2))


; PHYSICS ENGINE
; --------------
(define (physics-engine width height gravity floor-height)
  (let ((previous-time (current-time))
        (dt 0))
    
    (define (speed-average speed1 speed2)
      (let ((vx1 (ask speed1 get-x))
            (vy1 (ask speed1 get-y))
            (vx2 (ask speed2 get-x))
            (vy2 (ask speed2 get-y)))
        (coordinates (avg vx1 vx2)
                     (avg vy1 vy2))))
    
    ;calculate new position, based on given position & speed
    (define (update-position position speed)
      (let ((px (ask position get-x))
            (py (ask position get-y))
            (vx (ask speed get-x))
            (vy (ask speed get-y)))
        (coordinates (+ px (* vx dt)) 
                     (+ py (* vy dt)))))
    
    ;change the speed, based on gravity
    (define (update-speed speed)
      (coordinates (ask speed get-x)
                   (- (ask speed get-y) (* gravity dt))))
    
    (define (update-time) ;renew ∂t
      (let ((time (current-time)))
        (set! dt (- time previous-time))
        (set! previous-time time)))
    
    (define (set-physics-values! grav floor)
      (set! gravity grav)
      (set! floor-height floor))
    
    (define (update-avatar avatar)
      (let* ((position (ask avatar get-position))
             (speed (ask avatar get-speed))
             (updated-speed (update-speed speed))
             (speed-avg (speed-average speed updated-speed))
             (updated-position (update-position position speed-avg)))
        (cond ((>= (ask updated-position get-y) floor-height)
               (ask avatar set-position! updated-position)
               (ask avatar set-speed! updated-speed))
              (else (ask avatar start-running! floor-height)))))
    
    (define (float-avatar avatar) 
      (let* ((position (ask avatar get-position))
             (py (ask position get-y))
             (updated-py (- py (* 50 dt))))        
        (cond ((>= updated-py floor-height)
               (ask position set-y! updated-py))
              (else (ask avatar start-running! floor-height)))))
    
    (define (move-avatar avatar)
      (let ((status (ask avatar get-status)))
        (cond ((eq? status 'running) 'ignore) 
              ((eq? status 'jumping) (update-avatar avatar))
              ((eq? status 'floating) (float-avatar avatar))
              ((not status) (update-avatar avatar)))))
    
    (define (move-powerup bonus)
      (let* ((position (ask bonus get-position))
             (py (ask position get-y))
             (speed (ask bonus get-speed))
             (updated-position (update-position position speed)))
        (ask bonus set-position! updated-position)
        (if (< py floor-height) (ask position set-y! floor-height)))) 
    
    (define (move-obstacle obstacle) 
      (let* ((position (ask obstacle get-position))
             (speed (ask obstacle get-speed))
             (updated-position (update-position position speed)))
        (ask obstacle set-position! updated-position)))
    
    (define (move-zombie zombie) 
      (let* ((position (ask zombie get-position))
             (speed (ask zombie get-speed))
             (updated-position (update-position position speed)))
        (ask zombie set-position! updated-position)
        (ask updated-position set-y! floor-height)))
    
    (define (move-projectile bullet)
      (let* ((position (ask bullet get-position))
             (speed (ask bullet get-speed))
             (updated-position (update-position position speed)))
        (ask bullet set-position! updated-position)))
    
    (define (move object)
      (let ((type (ask object get-object-type)))
        (cond ((eq? type 'avatar) (move-avatar object)) 
              ((eq? type 'obstacle) (move-obstacle object))
              ((eq? type 'zombie) (move-zombie object))
              ((eq? type 'bullet) (move-projectile object))
              ((eq? type 'ammo-bonus) (move-powerup object))
              ((eq? type 'score-bonus) (move-powerup object)))))
    
    (dispatcher move
                update-time 
                set-physics-values!)))