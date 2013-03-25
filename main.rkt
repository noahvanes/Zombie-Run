;MAIN.RKT
;========

#lang r5rs

(#%require (only racket/base error when unless sleep collect-garbage))
(#%require "lib/all.rkt"
           "lib/external.rkt")

;***********************************************************
;* MAIN                                                    *
;* - The main 'executable' of this program                 *
;* - Contains the main ADT, the game-loop                  *
;*                                                         * 
;* - Based on a template by SOFT [Software Languages Lab]  * 
;* - By Noah Van Es                                        *
;***********************************************************

; GAME LOOP 
; ---------
(define (game-loop avatar window background)
  (let* ((width (ask window get-width))
         (height (ask window get-height))
         (floor 10)
         (engine (physics-engine width height 1000 floor))
         (current-level (level 1 width height 9 1 floor background black))
         (game-objects (queue))
         (projectiles (queue))
         (score (scoreboard 0 1 (data-file scoreboard-path)))
         (user-input #f)
         (delta-timer (milli-timer))
         (delta 15)) ;to optimize framerate (cfr. code report)
    
    (define dispatch-gameloop
      (dispatcher add-object!
                  add-projectile!
                  load-level
                  start
                  stop))
    
    (define (process-input)
      (let ((event (ask user-input last-recorded-event)))
        (cond ((eq? event 'up) (ask avatar jump!)
                               (ask user-input clear))
              ((eq? event 'down) (ask avatar float!))
              ((eq? event #\space) (ask avatar shoot! dispatch-gameloop)
                                   (ask user-input clear))
              ((eq? event 'release) (ask avatar clear-status!)
                                    (ask user-input clear)))))
    
    (define (collision-checks)
      (unless (ask game-objects empty?)
        (let* ((first-object (ask game-objects first))
               (object-type (ask first-object get-object-type))
               (avatar-hit? (ask avatar collision? first-object)))
          (cond ((and avatar-hit? (eq? object-type 'ammo-bonus)) 
                 (ask first-object activate avatar)
                 (ask game-objects serve!))
                ((and avatar-hit? (eq? object-type 'score-bonus)) 
                 (ask first-object activate score)
                 (ask game-objects serve!))                       
                (avatar-hit? (game-over))
                ((not (ask projectiles empty?))
                 (let* ((first-projectile (ask projectiles first))
                        (projectile-hit? (ask first-projectile hit? first-object)))
                   (cond ((and projectile-hit? (eq? object-type 'zombie))
                          (ask projectiles serve!)
                          (ask game-objects serve!))
                         (projectile-hit? (ask projectiles serve!)))))))))
    
    (define (wait)
      (let ((time-over (- delta (ask delta-timer time-elapsed))))
        (if (> time-over 0)
            (sleep (/ time-over 1000)))))
    
    (define (time-left?)
      (< (ask delta-timer time-elapsed) delta))
    
    (define (update)
      
      (ask delta-timer reset!) 
      (ask engine update-time)
      
      (for-each (lambda (object) 
                  (ask engine move object)) 
                (ask game-objects get-contents))
      
      (for-each (lambda (bullet)
                  (ask engine move bullet))
                (ask projectiles get-contents))
      
      (process-input)
      (ask engine move avatar)
      (collision-checks)  
      (ask score update-score)
      (ask current-level update dispatch-gameloop)
      (clean-game-objects!)
      (clean-projectiles!))
    
    (define (draw)
      
      (ask window clear)
      (ask window draw-simple-text 
           (coordinates 20 (- height 50))
           (string-append "score " (number->string (ask score get-score)))
           white)
      (ask window draw-simple-text 
           (coordinates (- width 80) (- height 50)) 
           (string-append "level: " (number->string (ask current-level get-difficulty)))
           white)
      (ask window draw-simple-text
           (coordinates 20 (- height 75))
           (string-append "ammo: " (number->string (ask avatar get-ammo)))
           white)
      
      (for-each (lambda (object)
                  (ask window draw-object object))
                (ask game-objects get-contents))
      
      (for-each (lambda (bullet)
                  (ask window draw-object bullet))
                (ask projectiles get-contents))
      
      (ask window draw-object avatar)
      (ask window refresh))
    
    ;the world-updater
    (define (game-advancer)
      
      (update)
      (when (time-left?)
        (draw) 
        (wait)))
    
    (define (wait-for-quit)
      (let ((event (ask user-input last-recorded-event)))
        (when (eq? event #\return)
          (ask window stop-primary-loop!)
          (open-menu)
          (ask window close))))
    
    (define (game-over)
      (stop)
      (sleep (/ delta 1000))
      (draw)
      (sleep (/ delta 1000))
      (define myscore (ask score get-score))
      (define myname (ask avatar get-name))
      (define x-offset (- (/ width 2) 100))
      (define y-offset (+ (/ height 2) 55))
      (ask window draw-text 
           (coordinates (- (/ width 2) 230) (+ y-offset 55)) 
           "Press enter to return to menu" 
           'small)
      (ask window draw-simple-text 
           (coordinates x-offset y-offset) 
           "GAME OVER"
           white)
      (ask window draw-simple-text 
           (coordinates x-offset (- y-offset 25)) 
           (string-append "YOUR SCORE IS  " (number->string myscore)) 
           white) 
      (let ((highscore (ask score get-highscore)))
        (cond ((or (not (number? highscore)) (> myscore highscore))
               (ask score insert-highscore! myname myscore)
               (ask window draw-simple-text (coordinates x-offset (- y-offset 50)) "NEW HIGHSCORE!" red))
              (else (ask score insert-highscore! myname myscore))))
      (ask window refresh)) 
    
    (define (start)
      (ask window open) 
      (set! user-input (ask window record-events!))
      (load-level current-level)
      (ask window start-background-loop! game-advancer)
      (ask engine update-time))
    
    (define (stop)
      (ask window stop-background-loop!)
      (ask window start-primary-loop! wait-for-quit))
    
    (define (load-level level)
      (set! current-level level)
      (ask current-level set-physics! avatar engine)
      (ask window install-background! (ask current-level get-background-image)))
    
    (define (add-object! object)
      (ask game-objects enqueue! object))
    
    (define (add-projectile! bullet)
      (ask projectiles enqueue! bullet))
    
    (define (clean-game-objects!)
      (unless (ask game-objects empty?)
        (let* ((object (ask game-objects first))
               (position (ask object get-position))
               (px (ask position get-x))
               (o-width (ask object get-width)))
          (if (< (+ px o-width) 0)
              (ask game-objects serve!)))))
    
    (define (clean-projectiles!)
      (unless (ask projectiles empty?)
        (let* ((first (ask projectiles first))
               (position (ask first get-position))
               (px (ask position get-x)))
          (if (> px width)
              (ask projectiles serve!)))))
    
    dispatch-gameloop))


; MENU
; ----
(define (menu window background options)
  (let* ((width (ask window get-width))
         (height (ask window get-height))
         (input-recorder #f))
    
    (define (initialize)
      (ask window install-background! background)
      (for-each (lambda (option) 
                  (ask window draw-option option #f #t))
                options))
    
    (define (menu-updater)
      
      (ask window refresh)
      (ask window clear)
      
      (define mouse-click (ask input-recorder get-last-click))
      (define mouse-hover (ask input-recorder get-mouse-position))
      
      ;check for event trigger
      (for-each (lambda (option)
                  (cond ((ask option hover? mouse-click) 
                         (ask option activate)
                         (close)) 
                        ((ask option hover? mouse-hover) 
                         (ask window draw-option option #t))))
                options)) 
    
    (define (open) 
      (ask window open)
      (set! input-recorder (ask window record-events!))
      (ask window start-primary-loop! menu-updater)
      (ask window refresh))
    
    (define (close)
      (ask window refresh)
      (ask window stop-primary-loop!)
      (ask window close))
    
    (initialize)
    
    (define (dispatch msg . pm)
      (cond ((eq? msg 'open) (open))
            ((eq? msg 'close) (close))
            (else (error 'menu "unknown message ~a" msg))))
    dispatch))

; INPUT-PROMPT
; ------------
(define (input-prompt window background)
  (let* ((width (ask window get-width))
         (height (ask window get-height))
         (input-recorder #f)
         (name "")
         (options '()))
    
    (define back (option-box "back"
                             (lambda () (open-menu))
                             (coordinates 35 25)
                             220 75
                             (custom-color 200 200 255)))
    (define start (option-box "start"
                              (lambda () (start-game-session name))
                              (coordinates 280 25)
                              275 75
                              (custom-color 200 255 200)))
    
    (define (initialize)
      (ask window install-background! background)
      (set! options (list back start))
      (for-each (lambda (option) 
                  (ask window draw-option option #f #t))
                options))
    
    (define (prompt-updater)
      
      (define (process-input)
        (let ((event (ask input-recorder last-recorded-event)))
          (cond ((not event) 'ignore)
                ((eq? event #\backspace)
                 (unless (equal? name "")
                   (set! name (substring name 0 (- (string-length name) 1)))))
                ((eq? event #\return) (ask start activate) (close))
                ((char? event) (set! name (string-append name (string event)))))))
      
      (ask window refresh)
      (ask window clear)
      
      (define mouse-click (ask input-recorder get-last-click))
      (define mouse-hover (ask input-recorder get-mouse-position))
      
      ;check for event trigger
      (for-each (lambda (option)
                  (cond ((ask option hover? mouse-click) 
                         (ask option activate)
                         (close)) 
                        ((ask option hover? mouse-hover) 
                         (ask window draw-option option #t))))
                options)
      
      (ask window draw-text (coordinates 25 (- height 110)) name 'big)
      
      (process-input)
      (ask input-recorder clear))
    
    (define (open) 
      (ask window open)
      (set! input-recorder (ask window record-events!))
      (ask window start-primary-loop! prompt-updater)
      (ask window clear)
      (ask window refresh))
    
    (define (close)
      (ask window refresh)
      (ask window stop-primary-loop!)
      (ask window close))
    
    (initialize)
    
    (define dispatch-prompt
      (dispatcher open 
                  close))
    dispatch-prompt))

; MESSAGE BOX
; -----------
(define (message-box window background messages)
  (let* ((width (ask window get-width))
         (height (ask window get-height))
         (input-recorder #f)
         (back (option-box "ok" 
                           (lambda () (open-menu)) 
                           (coordinates (- (/ width 2) 60) 25)
                           115 75
                           (custom-color 200 200 255))))
    
    (define (initialize)
      (ask window install-background! background)
      (ask window draw-option back #f #t)
      (do ((current messages (cdr current))
           (py-offset 60 (+ py-offset 31)))
        ((null? current) 'done)
        (ask window draw-text 
             (coordinates 60 (- height py-offset))
             (car current)
             'small #t)))
    
    (define (msgbox-updater)
      
      (ask window refresh)
      (ask window clear)
      
      (define mouse-click (ask input-recorder get-last-click))
      (define mouse-hover (ask input-recorder get-mouse-position))
      
      
      ;check for event trigger
      (cond ((ask back hover? mouse-click) 
             (ask back activate)
             (close)) 
            ((ask back hover? mouse-hover) 
             (ask window draw-option back #t))))
    
    (define (open) 
      (ask window open)
      (set! input-recorder (ask window record-events!))
      (ask window start-primary-loop! msgbox-updater)
      (ask window clear)
      (ask window refresh))
    
    (define (close)
      (ask window refresh)
      (ask window stop-primary-loop!)
      (ask window close))
    
    (initialize)
    
    (define dispatch-msgbox
      (dispatcher open 
                  close))
    dispatch-msgbox))


; GAME INITIALIZATION
; -------------------
(define (start-game-session name)
  (ask (game-loop (avatar name 20 #f 79 100)
                  (canvas "Zombie Run" 800 600)
                  game-background)
       start))

(define (open-menu)
  (ask (menu (canvas "Main Menu" 600 450)
             menu-background
             (list option1 option2 option3 option4))
       open))

(define (open-prompt)
  (ask (input-prompt (canvas "Please enter your name..." 600 250)
                     prompt-background)
       open))

(define (open-help)
  (ask (message-box (canvas "Instructions" 600 250)
                    msgbox-background
                    (list "press up to jump over objects"
                          "shoot zombies using spacebar"
                          "hold down the down arrow to float"))
       open))

(define (open-highscores)
  (let* ((score (scoreboard 0 1 (data-file scoreboard-path)))
         (highscores (ask score highscores->strings 3 30)))
    (ask (message-box (canvas "Highscores" 600 250)
                      msgbox-background
                      highscores)
         open)))

(define button-color (custom-color 235 235 235))
(define option1 (option-box "play" 
                            open-prompt
                            (coordinates 35 335)
                            220 75
                            button-color))

(define option2 (option-box "help" 
                            open-help
                            (coordinates 35 235)
                            220 75
                            button-color))

(define option3 (option-box "top3" 
                            open-highscores
                            (coordinates 35 135)
                            220 75
                            button-color))

(define option4 (option-box "quit" 
                            (lambda () 
                              (collect-garbage) 
                              (close-graphics) 
                              (display "[Game Ended]")
                              (newline))
                            (coordinates 35 35)
                            220 75
                            button-color))

(open-menu)