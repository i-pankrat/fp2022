(define iseven (lambda (x)
  (if (= x 0)
    #t
    (isodd (- x 1)))))
(define isodd (lambda (x) 
  (if (= x 0)
    #f
    (iseven (- x 1)))))

(display (iseven 12))
(display (isodd 12))