;;random.rkt

(save-as "random"

(define (random-generator init m a)
  (let ((seed (modulo (* init a) m)))
    
    (define (reset! new-seed)
      (set! seed (modulo new-seed m))
      (set! seed (modulo (* seed a) m)))
    
    (define (calibrate)
      (let ((user-input (event-recorder (make-pin J_PRESS 'press))))
        (do ((time 8 (+ time 1)))
          ((eq? (ask user-input last-event) 'press) (reset! time))
          (ask user-input read-input))))
    
    (define (generate-number)
      (set! seed (modulo (* seed a) m))
      (exact->inexact (/ seed m)))
    
    (define (random-int a b)
      (let* ((diff (- b a))
             (delta (* diff (generate-number))))
        (int (+ a delta))))
    
    (define (dispatch msg . pm)
      (cond ((eq? msg 'random-int) (apply random-int pm)) 
            ((eq? msg 'reset!) (apply reset! pm))
            ((eq? msg 'calibrate) (calibrate))
            ((eq? msg 'generate-number) (generate-number))
            (else (message-error 'random-generator msg))))
    dispatch))

(define r (random-generator 13 (- (expt 2 32) 1) (expt 7 5)))
(define (calibrate-randomizer) (ask r calibrate))
(define (random-int a b) (ask r random-int a b))

)