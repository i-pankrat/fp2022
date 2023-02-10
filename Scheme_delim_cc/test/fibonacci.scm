(define fib 
  (lambda (x)
    (if (< x 2)
      1
      (+ (fib (- x 1)) (fib (- x 2))))))
(display (fib 0) (fib 1) (fib 2) (fib 3) (fib 4)
         (fib 5) (fib 6) (fib 7) (fib 8) (fib 9))