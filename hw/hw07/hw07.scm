(define (square n) (* n n))

(define (pow base exp)
  (cond
    ((zero? exp) 1)
    ((even? exp) (square (pow base (floor (/ exp 2)))))
    ((odd? exp) (* base (square (pow base (floor (/ exp 2))))))))

(define (repeatedly-cube n x)
  (if (zero? n)
      x
      (let (
        (y (repeatedly-cube (- n 1) x))
        )
        (* y y y))))

(define (cddr s) (cdr (cdr s)))

; Returns the 2nd element of list s.
(define (cadr s) (car (cdr s)))

; Returns the 3rd element of list s.
(define (caddr s) (car (cddr s)))
