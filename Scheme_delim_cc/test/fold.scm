(define fold (lambda (f init list)
  (if (empty? list)
    init  
    (fold f (f init (car list)) (cdr list))))) 
    
(display (fold * 1 '(2 3 7)))
(display (fold 
  (lambda (s x)
    (if (> x 0) 
      (+ s x)
      s))
  0
  '(1 -2 3 -4 5 -6 7 -8)))