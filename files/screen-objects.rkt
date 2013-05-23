;;screen-objects.rkt

(save-as "avatar"
         
         (define (avatar x y w h c)
           (let ((posn (position x y)))
             
             (define (up! dy max-y)
               (if (< (+ (ask posn get-y) h) (- max-y dy))
                   (ask posn op-y! + dy)))
             (define (down! dy min-y)
               (if (> (ask posn get-y) (+ min-y dy))
                   (ask posn op-y! - dy)))
             
             (define (draw canvas)
               (let ((px (ask posn get-x))
                     (py (ask posn get-y)))
                 (ask canvas draw-rectangle px py w h c)))
             
             (define (collision? object)
               (define (horizontal-overlap?)
                 (let* ((av-px (ask posn get-x))
                        (av-px-end (+ av-px w))
                        (o-posn (ask object get-posn))
                        (o-px (ask o-posn get-x))
                        (o-w (ask object get-w))
                        (o-px-end (+ o-px o-w)))
                   (and (> av-px-end o-px) (< av-px o-px-end))))
               (define (vertical-overlap?)
                 (let* ((av-py (ask posn get-y))
                        (av-py-end (+ av-py h))
                        (o-posn (ask object get-posn))
                        (o-py (ask o-posn get-y))
                        (o-h (ask object get-h))
                        (o-py-end (+ o-py o-h)))
                   (and (> av-py-end o-py) (< av-py o-py-end))))
               (and (horizontal-overlap?) (vertical-overlap?))) 
             
             (define (move-between py-min py-max input dy)
               (let ((ev (ask input last-event)))
                 (cond ((eq? ev 'up) (up! dy py-max))
                       ((eq? ev 'down) (down! dy py-min)))))
             
             (define (dispatch msg . pm)
               (cond ((eq? msg 'draw) (apply draw pm))
                     ((eq? msg 'collision?) (apply collision? pm))
                     ((eq? msg 'move-between) (apply move-between pm))
                     (else (message-error 'avatar msg))))
             dispatch))
         )

(save-as "obstacle" 
         
         (define (obstacle x y w h c)
           (let ((posn (position x y)))
             
             (define (get-posn) posn)
             (define (get-w) w)
             (define (get-h) h)
             
             (define (move px width height)
               (ask posn op-x! - px)
               (if (< (+ w (ask posn get-x)) 0)
                   (respawn width height)))
             
             (define (respawn width height)
               (set! w (random-int 30 50))
               (set! h (random-int 30 80))
               (ask posn set-y! (random-int 0 (- height h)))
               (ask posn set-x! (- (* 3/2 width) w)))
             
             (define (draw canvas)
               (let ((px (ask posn get-x))
                     (py (ask posn get-y)))
                 (ask canvas draw-rectangle px py w h c)))
             
             (define (dispatch msg . pm)
               (cond ((eq? msg 'draw) (apply draw pm))
                     ((eq? msg 'move) (apply move pm))
                     ((eq? msg 'get-posn) (get-posn))
                     ((eq? msg 'get-w) (get-w))
                     ((eq? msg 'get-h) (get-h))
                     (else (message-error 'obstacle msg))))
             dispatch))
         
         )