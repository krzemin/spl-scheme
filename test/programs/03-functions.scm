(defun (range s t)
  (letrec range1 st
    (if (> (car st) (cdr st))
      (quote ())
      (cons (car st) (range1 (cons (+ 1 (car st)) (cdr st))))
    )
  (range1 (cons s t)))
)

(defun (map f xs)
  (letrec map1 xs1 
    (if (nil? xs1)
      xs1
      (cons (f (car xs1)) (map1 (cdr xs1)))
    )
  (map1 xs))
)

(defun (fib n)
  (letrec fib1 n1
    (if (< n1 2)
      n1
      (+ (fib1 (- n1 1)) (fib1 (- n1 2)))
    )
  (fib1 n))
)

(map fib (range 1 20))
