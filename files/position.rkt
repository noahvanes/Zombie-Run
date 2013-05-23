;;position.rkt

(save-as "position"

(define (position x y)
  
  (define (get-x) x)
  (define (get-y) y)
  
  (define (set-x! v) 
    (set! x v))
  (define (set-y! v)
    (set! y v))
  
  (define (op-x! op v)
    (set-x! (op x v)))
  (define (op-y! op v)
    (set-y! (op y v)))
  
  (define (dispatch msg . pm)
    (cond ((eq? msg 'get-x) (get-x))
          ((eq? msg 'get-y) (get-y))
          ((eq? msg 'set-x!) (apply set-x! pm))
          ((eq? msg 'set-y!) (apply set-y! pm))
          ((eq? msg 'op-x!) (apply op-x! pm))
          ((eq? msg 'op-y!) (apply op-y! pm))
          (else (message-error 'position msg))))
  dispatch)

)