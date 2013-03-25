;LIB/EXTERNAL.RKT
;================

#lang r5rs

(#%provide (all-defined))
(define STANDALONE? #f)

;************************************************************
;* EXTERNAL                                                 *
;* - Contains all paths to external files like images etc.  *
;*                                                          *
;*                                      - By Noah Van Es    *
;************************************************************

;;procedures
(define (make-resource-tracker)
  (if STANDALONE? 
      (lambda (x) 
        (string-append "/Applications/ZombieRun.app/Contents/Resources/" x))
      (lambda (x) 
        x)))

;this way, the case analysis only needs to be executed once
(define resource-path (make-resource-tracker))

(define (letter->image-path letter size)
  (if (eq? size 'big)
      (string-append (resource-path "letters/big/") (string letter) ".png")
      (string-append (resource-path "letters/small/") (string letter) ".png")))

; EXTERNAL FILES
; --------------
(define running-avatar (map resource-path (list "img/avatar/phase1.png"
                                                "img/avatar/phase2.png"
                                                "img/avatar/phase3.png"
                                                "img/avatar/phase4.png"
                                                "img/avatar/phase5.png"
                                                "img/avatar/phase6.png"
                                                "img/avatar/phase7.png"
                                                "img/avatar/phase8.png"
                                                "img/avatar/phase9.png"
                                                "img/avatar/phase10.png"
                                                "img/avatar/phase11.png")))
(define jumping-avatar (resource-path "img/avatar/jumping.png"))
(define floating-avatar (resource-path "img/avatar/floating.png"))
(define zombie-image (resource-path "img/zombie.png"))
(define bonus-image (resource-path "img/bonus.png"))
(define menu-background (resource-path "img/backgrounds/menu-background.png"))
(define game-background (resource-path "img/backgrounds/background.jpg"))
(define prompt-background (resource-path "img/backgrounds/prompt-background.png"))
(define msgbox-background (resource-path "img/backgrounds/msgbox-background.png"))
(define scoreboard-path (resource-path "data/highscores.rec"))