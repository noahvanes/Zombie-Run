;;event-recorder.rkt

(save-as "event-recorder"

(define (make-pin pin name) (cons pin name))
(define (get-pin pin) (car pin))
(define (get-name pin) (cdr pin))

(define (event-recorder . pins)
  (let ((event #f))
    
    (define (init)
      (for-each (lambda (p)
                  (enable-io-pin! PIOA (get-pin p))
                  (set-as-input-pin! PIOA (get-pin p)))
                pins))
    
    (define (register-event! p) (set! event p))
    (define (clear) (set! event #f))
    
    (define (read-input)
      (for-each (lambda (p)
                  (if (not (pin-set? PIOA (get-pin p)))
                      (register-event! (get-name p))))
                pins))
    
    (define (wait-for-input in act)
      (clear)
      (let loop ()
        (read-input)
        (cond ((eq? event in) (clear) (act))
               (else (loop)))))
    
    (define (last-event) event)
    
    (init)
      
    (define (dispatch msg . pm)
      (cond ((eq? msg 'read-input) (read-input))
            ((eq? msg 'last-event) (last-event))
            ((eq? msg 'clear) (clear))
            ((eq? msg 'wait-for-input) (apply wait-for-input pm))
            (else (message-error 'event-recorder msg))))
    dispatch))

)