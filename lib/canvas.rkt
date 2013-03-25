;LIB/CANVAS.RKT
;==============

#lang racket

(#%require (only racket/base error)
           "OO-syntactic-sugar.rkt"
           "event-recorder.rkt"
           "graphics-engine.rkt")

(#%provide canvas)

;***********************************************
;* CANVAS                                      *
;* - Contains canvas that represents a window  *
;*                                             *
;*                         - By Noah Van Es    *
;***********************************************

(define (singleton? lst)
  (and (pair? lst)
       (= (length lst) 1)))

(define (get singleton)
  (car singleton))

; CANVAS  
; ------
(define (canvas title width height . c)
  (let* ((engine (graphics-engine width height)) ;object renderer
         (buffer-pixmap (ask engine make-pixmap title))     
         (background-pixmap (ask engine make-pixmap title))                
         (color (if (singleton? c) (get c) white)) ;default: white
         (viewport #f))
    
    (define (initialize) 
      (ask engine draw-background color background-pixmap)
      (clear))
    
    (define (get-width) width)
    (define (get-height) height)
    
    (define (clear)
      (ask engine copy-contents background-pixmap buffer-pixmap))
    
    (define (refresh)
      (ask engine copy-contents buffer-pixmap viewport))
    
    (define (open)
      (when viewport (close))
      (set! viewport (ask engine make-viewport title))
      (refresh))
    
    (define (close)
      (ask engine uninstall-loop! viewport)
      (ask engine close viewport)
      (set! viewport #f))
    
    (define (draw-object object . on-background?)
      (let ((viewport (cond ((not (singleton? on-background?)) buffer-pixmap)
                            ((get on-background?) background-pixmap)
                            (else buffer-pixmap)))
            (type (ask object get-object-type)))
        (cond ((eq? type 'obstacle) (ask engine draw-obstacle object viewport))
              ((eq? type 'avatar) (ask engine draw-avatar object viewport))
              ((eq? type 'zombie) (ask engine draw-zombie object viewport))
              ((eq? type 'bullet) (ask engine draw-projectile object viewport))
              ((eq? type 'ammo-bonus) (ask engine draw-powerup object viewport))
              ((eq? type 'score-bonus) 
               (ask engine draw-powerup object viewport)))))
    
    (define (draw-option option highlighted? . on-background?)
      (let ((viewport (cond ((not (singleton? on-background?)) buffer-pixmap)
                            ((get on-background?) background-pixmap)
                            (else buffer-pixmap))))
        (ask engine draw-option option highlighted? viewport)))
    
    (define (draw-text position string size . on-background?)
      (let ((viewport (cond ((not (singleton? on-background?)) buffer-pixmap)
                            ((get on-background?) background-pixmap)
                            (else buffer-pixmap))))
        (ask engine draw-text position string size viewport)))
    
    (define (draw-simple-text position string color . on-background?)
      (let ((viewport (cond ((not (singleton? on-background?)) buffer-pixmap)
                            ((get on-background?) background-pixmap)
                            (else buffer-pixmap))))
        (ask engine draw-simple-text position string color viewport)))
    
    (define (install-background! image)
      (ask engine clear-contents background-pixmap)
      (ask engine draw-background image background-pixmap))
    
    (define (start-primary-loop! advancer)
      (when viewport
        (ask engine install-loop! viewport advancer)))
    
    (define (stop-primary-loop!)
      (ask engine uninstall-loop! viewport))
    
    (define (start-background-loop! advancer)
      (ask engine install-loop! buffer-pixmap advancer))
    
    (define (stop-background-loop!) 
      (ask engine uninstall-loop! buffer-pixmap))
    
    (define (record-events!)
      (let ((recorder (event-recorder viewport width height)))
        (ask engine write-keystrokes! viewport recorder)
        recorder))
    
    (initialize)
    
    (define dispatch-canvas
      (dispatcher clear 
                  refresh
                  draw-object
                  draw-option
                  draw-text
                  draw-simple-text
                  open
                  close
                  get-width
                  get-height
                  record-events!
                  install-background!
                  start-primary-loop!
                  stop-primary-loop!
                  start-background-loop!
                  stop-background-loop!))
    dispatch-canvas))