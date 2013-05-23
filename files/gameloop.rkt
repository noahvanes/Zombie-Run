;;gameloop.rkt

(save-as "gameloop"
         
         (define (gameloop av obs f w h speed c1 c2)
           (let ((main-ui (canvas-ui 0 f w h c1))
                 (bottom (canvas-ui 0 0 w f c2))
                 (user-input (event-recorder (make-pin J_UP 'up) (make-pin J_DOWN 'down) (make-pin J_LEFT 'left) (make-pin J_RIGHT 'right)))
                 (score (scoreboard speed))
                 (status #t))
             
             (define (start)
               
               (define (update)
                 (ask score update)
                 (ask user-input clear)
                 (ask user-input read-input)
                 (ask av move-between 0 h user-input speed)
                 (if (eq? (ask user-input last-event) 'left) (pauze-game)) 
                 (for-each (lambda (obst)
                             (ask obst move speed w h)
                             (if (ask av collision? obst)
                                 (game-over)))
                           obs))
               
               (define (draw)
                 (ask main-ui clear)
                 (for-each (lambda (obst)
                             (ask obst draw main-ui))
                           obs)
                 (ask av draw main-ui)
                 (ask main-ui refresh))
               
               (define (advance)
                 (cond ((eq? status 'pauzed) (pauze-game))
                       (status (update) (draw) (advance))))
               
               (define (pauze-game)
                 (set! status 'pauzed)
                 (ask main-ui draw-rectangle 25 15 30 80 WHITE)
                 (ask main-ui draw-rectangle 75 15 30 80 WHITE)
                 (ask main-ui refresh)
                 (ask user-input wait-for-input 'right 
                      (lambda () (set! status #t) (advance))))
                  
                  (define (game-over)
                    (set! status #f)
                    (print "Game Over"
                           (string-append "Your score was " (number->string (ask score get-score)))))
               
                  (advance))
                 
                 (define (dispatch msg . pm)
                   (cond ((eq? msg 'start) (start))
                         (else (message-error 'gameloop msg))))
                 
                 dispatch))
             )