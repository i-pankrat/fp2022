(define Y
  (lambda (f)
    ((lambda (g) (g g))
      (lambda (g)
        (f  (lambda a (apply (g g) a)))))))

(define fac
  (Y (lambda (r)
    (lambda (x)
      (if (< x 2)
        1
        (* x (r (- x 1))))))))

(define fib
  (Y (lambda (r)
    (lambda (x)
      (if (< x 2)
        1
        (+ (r (- x 2)) (r (- x 1))))))))

(display (fac 5))
(display (fib 10))