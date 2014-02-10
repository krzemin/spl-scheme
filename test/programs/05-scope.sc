(begin
  (define x 10)
  (defun (p y) (+ x y))
  (begin 
    (define x 15)
    (p 100)
  )
)