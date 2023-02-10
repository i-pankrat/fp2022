(define plus_x
  (lambda (n)
    (lambda (x)
      (+ x n))))
(define plus_one (lambda (x) ((plus_x 1) x)))
(define plus_two (lambda (x) ((plus_x 2) x)))
(display (plus_one 0) (plus_one 1) (plus_two 1) (plus_one 3) (plus_two 3))