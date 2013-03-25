;LIB/SCREEN-OBJECTS.RKT
;======================

#lang r5rs

(#%require "coordinates.rkt")
(#%require "OO-syntactic-sugar.rkt")
(#%require (only racket/base error unless when))
(#%provide avatar zombie obstacle ammo-bonus score-bonus)

;***************************************************************************
;* SCREEN OBJECTS                                                          *
;* - main on-screen objects                                                *
;* - currently includes avatar, obstacle, zombie, bullet and powerup/bonus *
;*                                                                         *
;*                                                       - By Noah Van Es  *
;***************************************************************************

(define pi (acos -1))

; AVATAR
; ------
(define (avatar name x vy width height)
  (let ((position (coordinates x 0))
        (speed (coordinates 0 0))
        (ammo 1)
        (status #f))
    
    (define (get-object-type) 'avatar)
    
    (define (get-position) position)
    (define (get-speed) speed)
    (define (get-width) width)
    (define (get-height) height)
    (define (get-ammo) ammo)
    (define (get-name) name)
    (define (get-status) status)
    (define (get-vy) vy)
    
    (define (set-position! p)
      (set! position p))
    (define (set-speed! s)
      (set! speed s))  
    (define (set-vy! v)
      (set! vy v))
    
    (define (start-running! floor)
      (ask position set-y! floor)
      (set! status 'running))
    
    (define (float!)
      (let ((current-vy (ask speed get-y)))
        (when (and (not (eq? status 'running)) (< current-vy 0))
          (ask speed set-x! 0)
          (ask speed set-y! 0)
          (set! status 'floating))))
    
    (define (jump!)
      (when (eq? status 'running)
        (ask speed set-y! vy)
        (set! status 'jumping)))
    
    (define (clear-status!)
      (set! status #f))
    
    (define (pickup-ammo! v)
      (set! ammo (+ ammo v))) 
    
    (define (shoot! gameloop)
      (if (> ammo 0)
          (let* ((px (ask position get-x))
                 (py (ask position get-y))
                 (px-fix (+ px width))
                 (py-fix (+ py (/ height 2) 5))
                 (projectile (bullet px-fix py-fix)))
            (ask gameloop add-projectile! projectile)
            (set! ammo (- ammo 1)))))
    
    (define (collision? object)
      (define (horizontal-overlap?)
        (let* ((av-px (ask position get-x))
               (av-px-end (+ av-px width))
               (o-pos (ask object get-position))
               (o-px (ask o-pos get-x))
               (o-width (ask object get-width))
               (o-px-end (+ o-px o-width)))
          (and (> av-px-end o-px) (< av-px o-px-end))))
      (define (vertical-overlap?)
        (let* ((av-py (ask position get-y))
               (av-py-end (+ av-py height))
               (o-pos (ask object get-position))
               (o-py (ask o-pos get-y))
               (o-height (ask object get-height))
               (o-py-end (+ o-py o-height)))
          (and (> av-py-end o-py) (< av-py o-py-end))))
      (and (horizontal-overlap?) (vertical-overlap?)))    
    
    (define dispatch-avatar
      (dispatcher start-running!
                  jump!
                  float!
                  clear-status!
                  get-object-type 
                  get-position
                  get-width
                  get-height
                  get-speed
                  get-ammo
                  get-status
                  get-vy
                  get-name
                  set-position!
                  set-speed!
                  set-vy!
                  pickup-ammo!
                  shoot!
                  collision?))
    dispatch-avatar))

; ZOMBIE
; ------
(define (zombie width height x x-speed)
  (let ((position (coordinates x 0))
        (speed (coordinates x-speed 0)))
    
    (define (get-object-type) 'zombie)
    
    (define (get-position) position)
    (define (get-speed) speed)
    (define (get-width) width)
    (define (get-height) height)
    
    (define (set-position! p)
      (set! position p))
    (define (set-speed! s)
      (set! speed s))  
    
    (dispatcher get-object-type
                get-position
                get-width
                get-height
                get-speed
                set-position!
                set-speed!)))

; BULLET
; ------
(define (bullet x y)
  (let ((position (coordinates x y))
        (speed (coordinates 400 0)))
    
    (define (get-object-type) 'bullet)
    
    (define (get-position) position)
    (define (get-speed) speed)
    
    (define (set-position! p)
      (set! position p))
    (define (set-speed! s)
      (set! speed s))
    
    (define (hit? object)
      (let* ((b-px (ask position get-x))
             (b-py (ask position get-y))
             (o-position (ask object get-position))
             (o-px (ask o-position get-x))
             (o-py (ask o-position get-y))
             (o-height (ask object get-height))
             (o-width (ask object get-width)))
        (and (>= b-px o-px) 
             (<= b-px (+ o-px o-width))
             (<= b-py (+ o-py o-height))
             (>= b-py o-py))))
    
    (dispatcher get-object-type
                get-position
                get-speed
                set-position!
                set-speed!
                hit?)))

; BONUS
; -----
(define (bonus kind width height x y x-speed action)
  (let ((position (coordinates x y))
        (speed (coordinates x-speed 0)))
    
    (define (get-object-type) kind)
    
    (define (get-position) position)
    (define (get-speed) speed)
    (define (get-width) width)
    (define (get-height) height)
    
    (define (set-position! p)
      (set! position p))
    (define (set-speed! s)
      (set! speed s))  
    
    (define (activate . pm)
      (apply action pm))
    
    (dispatcher get-object-type
                get-position
                get-speed
                get-width
                get-height
                set-position!
                set-speed!
                activate)))

;;ammo-bonus
(define (ammo-bonus width height x y x-speed amount)
  (let ((action (lambda (avatar) (ask avatar pickup-ammo! amount))))
    (bonus 'ammo-bonus width height x y x-speed action)))

;;score-bonus
(define (score-bonus width height x y x-speed amount)
  (let ((action (lambda (scoreboard) (ask scoreboard incr! amount))))
    (bonus 'score-bonus width height x y x-speed action)))

;;etc...

; OBSTACLE
; --------
(define (obstacle width height x x-speed color)
  (let ((position (coordinates x 0))
        (speed (coordinates x-speed 0)))
    
    (define (get-object-type) 'obstacle)
    
    (define (get-position) position)
    (define (get-speed) speed)
    (define (get-width) width)
    (define (get-height) height)
    (define (get-color) color)
    
    (define (set-position! p)
      (set! position p))
    (define (set-color! c)
      (set! color c))
    
    (dispatcher get-object-type
                get-position
                get-speed
                get-width
                get-height
                get-color
                set-position!
                set-color!)))