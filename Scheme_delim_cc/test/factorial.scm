(define factorial
  (lambda (x)
    (if (< x 1)
      1
      (* x (factorial (- x 1))))))

(display 
  (list (factorial 1) 
        (factorial 2) 
        (factorial 3) 
        (factorial 4) 
        (factorial 5)
        (factorial 6) 
        (factorial 7)))