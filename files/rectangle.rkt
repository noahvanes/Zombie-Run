;;rectangle.rkt

(save-as "rectangle"

(define (rectangle x1 y1 x2 y2 color)
  (vector x1 y1 x2 y2 color))

(define (get-x1 rect)
  (vector-ref rect 0))

(define (get-y1 rect)
  (vector-ref rect 1))

(define (get-x2 rect)
  (vector-ref rect 2))

(define (get-y2 rect)
  (vector-ref rect 3))

(define (get-color rect)
  (vector-ref rect 4))

(define (valid? rect)
  (and (< (get-x1 rect) (get-x2 rect))
       (< (get-y1 rect) (get-y2 rect))))

(define (left-edge old new color)
  (rectangle (get-x1 old)
             (get-y1 old)
             (get-x1 new)
             (get-y2 old)
             color))

(define (right-edge old new color)
  (rectangle (get-x2 new)
             (get-y1 old)
             (get-x2 old)
             (get-y2 old)
             color))

(define (top-edge old new color)
  (rectangle (get-x1 old)
             (get-y2 new)
             (get-x2 old)
             (get-y2 old)
             color))

(define (lower-edge old new color)
  (rectangle (get-x1 old)
             (get-y1 old)
             (get-x2 old)
             (get-y1 new)
             color))

(define (calc-edges old new color)
  (define lst '())
  (define t (top-edge old new color))
  (define r (right-edge old new color))
  (define b (lower-edge old new color))
  (define l (left-edge old new color))
  (for-each (lambda (edge) (if (valid? edge)
                               (set! lst (cons edge lst))))
            (list r t b l))
  lst)

(define (insert rect lst)
  (cond ((null? lst) (cons rect '()))
        ((< (get-x1 rect) (get-x1 (car lst)))
         (cons rect lst))
        (else (cons (car lst) (insert rect (cdr lst))))))
)