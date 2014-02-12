(defun (multlist xs)
  (call/cc (lambda k
      (letrec mt yss
        (if (nil? yss)
          1
          (let* (
              (y (car yss))
              (ys (cdr yss))
            )
            (if (= y 0)
              (throw k 0)
              (* y (mt ys))
            )
          )
        )
        (mt xs)
      )
    )
  )
)

(define v1 (multlist (cons 2 (cons 5 (cons 0 (cons 11 nil))))))
(define v2 (multlist (cons 2 (cons 5 (cons 7 (cons 11 nil))))))

(cons v1 v2)