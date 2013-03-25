;LIB/GRAPHICS-ENGINE.RKT
;=======================

#lang r5rs

(#%require "OO-syntactic-sugar.rkt"
           graphics/graphics
           racket/draw
           "animated-gif.rkt"
           "coordinates.rkt"
           "external.rkt"
           (only racket/base error))

(open-graphics) ;initiates the graphics library
(define FONT-SIZE 15)

(#%provide graphics-engine
           red
           green
           blue
           black 
           white
           grey
           light-grey
           custom-color
           close-graphics)

;*********************************************************************
;* GRAPHICS-ENGINE                                                   *
;* - Provides color support and easier drawing syntax                *
;* - Contains ADT for extra abstraction on graphics library          *
;* - This ADT can manipulate/create viewports/pixmaps of given size  *
;*                                                                   *
;*                                              - By Noah Van Es     *
;*********************************************************************

;; (cfr lib/animated-gif)
(define avatar-gif  
  (animated-gif running-avatar 0))

; COLOR SUPPORT
; -------------
;make-rgb is imported from graphics library
(define (custom-color r g b)
  (make-rgb (/ r 255) (/ g 255) (/ b 255)))

(define red (custom-color 255 0 0))
(define green (custom-color 0 255 0))
(define blue (custom-color 0 0 255))
(define black (custom-color 0 0 0))
(define white (custom-color 255 255 255))
(define grey (custom-color 24 24 24))
(define light-grey (custom-color 128 128 128))

; DRAWING PROCEDURES (with easier syntax)
; ------------------
(define (fill-rectangle! x y width height color viewport)
  ((draw-solid-rectangle viewport) (make-posn x y)
                                   width 
                                   height 
                                   color))

(define (draw-rectangle! x y width height color viewport)
  ((draw-rectangle viewport) (make-posn x y)
                             width 
                             height 
                             color))

(define (fill-ellipse! x y width height color viewport) 
  ((draw-solid-ellipse viewport) (make-posn x y)
                                 width
                                 height  
                                 color))

(define (draw-line! x1 y1 x2 y2 color viewport)
  ((draw-line viewport) (make-posn x1 y1)
                        (make-posn x2 y2)
                        color))

(define (draw-text! x y str color viewport)
  ((draw-string viewport) (make-posn x y)
                          str 
                          color))

(define (draw-image! x y image viewport)
  (((draw-pixmap-posn image) viewport) (make-posn x y)))

(define (put-pixel! x y color viewport)
  ((draw-pixel viewport) (make-posn x y) 
                         color))


; GRAPHICS-ENGINE  
; ---------------
(define (graphics-engine width height)
  
  (define (draw-avatar avatar viewport)
    (let* ((position (ask avatar get-position))
           (px (ask position get-x))
           (py (ask position get-y))
           (av-height (ask avatar get-height))
           (px-fix (+ px 2)) ;image correction
           (py-fix (- height (+ py av-height)))
           (status (ask avatar get-status))
           (image (cond ((eq? status 'running) 
                         (ask avatar-gif next-image!))
                        ((eq? status 'floating)
                         (set! py-fix (- height (+ py 150)))
                         floating-avatar)
                        (else jumping-avatar))))
      (draw-image! px-fix py-fix image viewport)))
  
  (define (draw-zombie zombie viewport)
    (let* ((position (ask zombie get-position))
           (px (ask position get-x))
           (py (ask position get-y))
           (z-height (ask zombie get-height))
           (px-fix (- px 7)) ;image correction
           (py-fix (- height (+ py z-height))))
      (draw-image! px-fix py-fix zombie-image viewport)))
  
  (define (draw-obstacle obstacle viewport)
    (let* ((position (ask obstacle get-position))
           (px (ask position get-x))
           (py (ask position get-y))
           (o-width (ask obstacle get-width))
           (o-height (ask obstacle get-height))
           (color (ask obstacle get-color))
           (py-fix (- height (+ py o-height))))
      (fill-rectangle! px py-fix
                       o-width o-height 
                       color 
                       viewport)))
  
  (define (draw-powerup bonus viewport)
    (let* ((position (ask bonus get-position))
           (px (ask position get-x))
           (py (ask position get-y))
           (b-height (ask bonus get-height))
           (py-fix (- height (+ py b-height))))
      (draw-image! px py-fix bonus-image viewport)))
             
  (define (draw-projectile bullet viewport)
    (let* ((position (ask bullet get-position))
           (px (ask position get-x))
           (py (ask position get-y))
           (py-fix (- height py)))
      (draw-line! px py-fix (+ px 5) py-fix white viewport)))
  
  (define (draw-option option highlighted? viewport)
    (let* ((position (ask option get-position))
           (px (ask position get-x))
           (py (ask position get-y))
           (o-width (ask option get-width))
           (o-height (ask option get-height))
           (py-fix (- height (+ py o-height)))
           (color (ask option get-hover-color))
           (text (ask option get-text)))
      (if highlighted?
          (fill-rectangle! px py-fix
                           o-width o-height
                           color 
                           viewport))
      (draw-rectangle! px py-fix
                       o-width o-height
                       black
                       viewport)
      (draw-text (coordinates (+ px 8) (+ py 10)) text 'big viewport))) 
  
  (define (draw-text position string size viewport)
    (let* ((px (ask position get-x))
           (py (ask position get-y))
           (font-size (if (eq? size 'big) 55 20))
           (py-fix (- height (+ py font-size)))
           (str-length (string-length string)))
      (do ((index 0 (+ index 1))
           (x-offset 0 (+ x-offset (- font-size 5))))
        ((= index str-length) 'done)
        (draw-image! (+ px x-offset) 
                     py-fix 
                     (letter->image-path (string-ref string index) size)
                     viewport))))
  
  (define (draw-simple-text position string color viewport)
    (let* ((px (ask position get-x))
           (py (ask position get-y))
           (py-fix (- height (+ py FONT-SIZE))))
      (draw-text! px py-fix string color viewport)))
  
  (define (draw-background background viewport)
    (if (string? background)
        (draw-image! 0 0 background viewport)
        (fill-rectangle! 0 0 width height background viewport)))
  
  (define (make-pixmap title)
    (open-pixmap title width height))
  
  (define (make-viewport title)
    (open-viewport title width height))
  
  (define (copy-contents v1 v2)
    (copy-viewport v1 v2))
  
  (define (clear-contents viewport)
    ((clear-viewport viewport)))
  
  (define (close viewport)
    (close-viewport viewport))
  
  (define (install-loop! viewport advancer)
    ((set-on-tick-event viewport) 1
                                  (lambda x (advancer))))
  
  (define (uninstall-loop! viewport)
    ((stop-tick viewport)))
  
  (define (write-keystrokes! viewport recorder)
      ((set-on-key-event viewport)  
       (lambda (keypress x) 
         (ask recorder register-key-event! (key-value keypress)))))
  
  (define dispatch-engine
    (dispatcher draw-obstacle
                draw-zombie
                draw-powerup
                draw-avatar
                draw-projectile
                draw-option
                draw-text
                draw-simple-text
                draw-background
                make-pixmap
                make-viewport
                copy-contents
                clear-contents
                close
                install-loop!
                uninstall-loop!
                write-keystrokes!))
  dispatch-engine)