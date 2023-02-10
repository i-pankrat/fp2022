(define map (lambda (f list)
  (if (empty? list)
    '()
    (cons (f (car list)) (map f (cdr list))))))

(define square (lambda (x) (* x x)))

(display (map square `(1 ,(+ 1 1) ,(* 1 3) ,(square 2) 5 6)))