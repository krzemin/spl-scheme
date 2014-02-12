(define x 10)
((begin
  (define x 20)
  (lambda y (+ x y))
) x) 
