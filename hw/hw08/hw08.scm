(define (ascending? s) 
  (if (<= (length s) 1)
      #t
      (let ((first (car s))
             (second (car (cdr s))))
        (and (<= first second)
             (ascending? (cdr s))))))

(define (my-filter pred s) 
    (if (null? s)
        '()
        (if (pred (car s))
            (cons (car s) (my-filter pred (cdr s)))
            (my-filter pred (cdr s)))))

(define (interleave lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      (append lst1 lst2)
      (cons (car lst1) (cons (car lst2) (interleave (cdr lst1) (cdr lst2))))))


(define (no-repeats s)
  (define (not-equal-to x)
    (lambda (y) (not (= x y))))

  (define (filter-non-repeats lst)
    (if (null? lst)
        '()
        (cons (car lst)
              (filter-non-repeats (filter (not-equal-to (car lst)) (cdr lst))))))

  (filter-non-repeats s))

