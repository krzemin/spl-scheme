
(defun if (b v1 v2) (
	cond (b v1) (#t v2)
))

(defun length lst (
	letrec (
		(length' (lambda (lst' acc) (
			if (empty? lst') acc
			(length' (tail lst') (+ 1 acc))
		)))
	)
	length' lst 0
))
