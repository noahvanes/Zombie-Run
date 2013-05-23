;;scoreboard.rkt

(save-as "scoreboard"
         
(define (scoreboard updater)
  (let ((score 0))
    
    (define (get-score) score)
    (define (reset!) (set! score 0))
    (define (set-updater! v)
      (set! updater v))
    
    (define (update)
      (set! score (+ score updater)))
    
    (define (dispatch msg . pm)
      (cond ((eq? msg 'update) (update)) 
            ((eq? msg 'get-score) (get-score))
            ((eq? msg 'reset!) (reset!))
            ((eq? msg 'set-updater!) (apply set-updater! pm))
            (else (message-error 'scoreboard msg))))
    dispatch))

)        