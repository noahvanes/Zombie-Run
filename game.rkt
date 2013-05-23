;;game.rkt

(save-as "game"
         
         (newline) (display "> loading...") (newline)
         
         (load "utils")
         (load "start-up")
         (load "scoreboard")
         (load "rectangle")
         (load "screen-data")
         (load "canvas-ui")
         (load "position")
         (load "event-recorder")
         (load "random")
         (load "avatar")
         (load "obstacle")
         (load "gameloop")
         
         (define (start-game-session)
           (ask (gameloop (avatar 13 50 10 10 BLUE)
                          (list (obstacle 130 20 40 60 BLACK)
                                (obstacle 230 50 30 50 BLACK))
                          15 130 115 5 #b111111110000 #b001010000101)
                start))
         
         (newline)
         
         (print "Welcome!"
                "Press to start a new game...")
         
         (newline)
                
         (calibrate-randomizer)
         (start-game-session)
         
         (let replay ()
           (newline)
           (print "Press to try again...")
           (let ((user-input (event-recorder (make-pin J_PRESS 'press))))
             (ask user-input wait-for-input 'press start-game-session)
             (replay)))
         
         )