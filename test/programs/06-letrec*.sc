(letrec* (
  (odd x
    (if (= x 0)
      #f
      (even (- x 1))
    )
  )
  (even x
    (if (= x 0)
      #t
      (odd (- x 1))
    )
  )
) (cons (cons (odd 4) (odd 5)) (cons (even 4) (even 5))))
