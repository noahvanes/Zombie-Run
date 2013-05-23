;;utils.rkt

(save-as "utils" 

(define-syntax save-as
  (syntax-rules ()
    ((save-as name exp1 ...)
     (let ((port (open-output-file name)))
       (write '(begin exp1 ...) port)
       (close-output-port port)))))         

(define-syntax ask
  (syntax-rules ()
    ((ask object command) (object 'command))
    ((ask object command p1 ...) (object 'command p1 ...))))

(define (message-error obj msg)
  (string-append (symbol->string obj)
                 "> unknown message: "
                 (symbol->string msg)))
         
(define-syntax when
  (syntax-rules ()
    ((when c b1 ...) 
     (if c 
         (begin b1 ...)))))

(define-syntax unless
  (syntax-rules ()
    ((when c b1 ...) 
     (if (not c) 
         (begin b1 ...)))))

(define (int x) (inexact->exact (round x)))
(define (square x) (* x x))
(define (double x) (+ x x))

(define (print . strings)
  (for-each (lambda (str)
              (display str)
              (newline))
            strings))

(define pi (acos -1))

)