;;global screen data
(define SCREEN-X 130)
(define SCREEN-Y 130)
(define (rev-y y) (- SCREEN-Y y))

;;colors 
(define WHITE #b111111111111)
(define BLACK #b000000000000)

;;ADT canvas-ui
(define (canvas-ui init-x init-y width height bg-color)
  (let ((onscreen '())
        (buffer '()))
    
    (define (pos-x x) (+ init-x x)) 
    (define (pos-y y) (rev-y (+ init-y y)))
    
    (define (clear) (set! buffer '()))
    
    (define (setup-bg) 
      (fill-rectangle! (pos-x 0) (pos-y height) width height bg-color))
    
    (define (draw-rectangle x y width height color)
      (define r (rectangle x y (+ x width) (+ y height) color))
      (set! buffer (insert r buffer)))
    
    (define (draw-rect! rect . c)
      (fill-rectangle! (pos-x (get-x1 rect))
                       (pos-y (get-y2 rect))
                       (- (get-x2 rect) (get-x1 rect))
                       (- (get-y2 rect) (get-y1 rect))
                       (if (null? c) (get-color rect) (car c))))
    
    (define (smart-draw new-rect old-rect)
      (draw-rect! new-rect)
      (if (or (> (get-y1 new-rect) (get-y2 old-rect))
              (< (get-y2 new-rect) (get-y1 old-rect)))
          (draw-rect! old-rect bg-color)
          (let* ((area (intersection new-rect old-rect #f))
                 (old-edges (calc-edges old-rect area bg-color)))
            (for-each (lambda (r) (draw-rect! r)) old-edges))))
    
    (define (refresh)
      (let loop ((c-scr onscreen)
                 (c-buf buffer))
        (cond ((null? c-scr) 
               (for-each (lambda (r) (draw-rect! r)) c-buf))
              ((null? c-buf) 
               (for-each (lambda (r) (draw-rect! r bg-color)) c-scr))
              ((< (get-x2 (car c-buf)) (get-x1 (car c-scr)))
               (draw-rect! (car c-buf))
               (loop c-scr (cdr c-buf)))
              ((> (get-x1 (car c-buf)) (get-x2 (car c-scr)))
               (draw-rect! (car c-scr) bg-color)
               (loop (cdr c-scr) c-buf))
              (else (smart-draw (car c-buf) (car c-scr))
                    (loop (cdr c-scr) (cdr c-buf)))))
      (set! onscreen (copy buffer)))
    
    (define (put-pixel x y color)
      (draw-rectangle x y 1 1 color))
    
    (define (draw-horizontal-line x y length color)
      (draw-rectangle x y length 1 color))
    
    (define (draw-vertical-line x y length color)
      (draw-rectangle x y 1 length color))
    
    (define (set-bg! c)
      (set! bg-color c)
      (setup-bg)
      (set! onscreen '())
      (clear))
    
    (setup-bg)
    
    (define (dispatch msg . pm)
      (cond ((eq? msg 'draw-rectangle) (apply draw-rectangle pm))
            ((eq? msg 'put-pixel) (apply put-pixel pm))
            ((eq? msg 'set-bg!) (apply set-bg! pm))
            ((eq? msg 'refresh) (refresh))
            ((eq? msg 'clear) (clear))
            (else (message-error 'canvas-ui msg))))
    dispatch))

(define main-ui (canvas-ui 0 0 130 130 WHITE))

(define (test)  
  (let loop ((current-x 100))
    (ask main-ui clear)
    (ask main-ui draw-rectangle current-x 0 30 70 BLACK)
    (ask main-ui refresh)
    (if (not (= current-x 0))
        (loop (- current-x 1)))))