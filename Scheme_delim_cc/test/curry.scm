(define plusxy (lambda (x y)
  (+ x y)))

(define curry (lambda (func first)
    (lambda arg
    (apply func (cons first arg)))))

(define plusone (curry plusxy 1))
(define plusten (curry plusxy 10))
(display (plusone 2))
(display (plusten 3))