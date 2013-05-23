;;canvas-ui.rkt

(save-as "screen-data"
         
         (define SCREEN-X 130)
         (define SCREEN-Y 130)
         (define (rev-y y) (- SCREEN-Y y))
         
         (define WHITE #b111111111111)
         (define BLACK #b000000000000)
         (define RED #b000000001111)
         (define BLUE #b111100000000)
         (define GREEN #b000011110000)
         
         )

(save-as "canvas-ui"
         
         (define (canvas-ui init-x init-y width height bg-color)
           (let ((onscreen '())
                 (buffer '()))
             
             (define (pos-x x) (+ init-x x)) 
             (define (pos-y y) (rev-y (+ init-y y)))
             
             (define (clear) (set! buffer '()))
             
             (define (draw-rectangle x y width height color)
               (define r (rectangle x y (+ x width) (+ y height) color))
               (set! buffer (insert r buffer)))
             
             (define (draw-rect! rect . c)
               (fill-rectangle! (int (pos-x (get-x1 rect)))
                                (int (pos-y (get-y2 rect)))
                                (int (- (get-x2 rect) (get-x1 rect)))
                                (int (- (get-y2 rect) (get-y1 rect)))
                                (if (null? c) (get-color rect) (car c))))
             
             (define (quickdraw new-rect old-rect)
               (draw-rect! new-rect)
               (if (or (> (get-y1 new-rect) (get-y2 old-rect))
                       (< (get-y2 new-rect) (get-y1 old-rect)))
                   (draw-rect! old-rect bg-color)
                   (let ((old-edges (calc-edges old-rect new-rect bg-color)))
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
                       (else (quickdraw (car c-buf) (car c-scr))
                             (loop (cdr c-scr) (cdr c-buf)))))
               (set! onscreen buffer))
             
             (fill-rectangle! (pos-x 0) (pos-y height) width height bg-color)
             
             (define (dispatch msg . pm)
               (cond ((eq? msg 'draw-rectangle) (apply draw-rectangle pm))
                     ((eq? msg 'refresh) (refresh))
                     ((eq? msg 'clear) (clear))
                     (else (message-error 'canvas-ui msg))))
             dispatch))
         
         )