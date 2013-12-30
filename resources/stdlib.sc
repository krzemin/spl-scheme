
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

(defun append (lst1 lst2) (
	if (empty? lst1) lst2
	(cons (head lst1) (append (tail lst1) lst2))
))

(defun reverse lst (
	if (empty? lst) lst
	(append (reverse (tail lst)) ((head lst)))
))

(defun foldr (f z lst) (
	if (empty? lst) z
	(f (head lst) (foldr f z (tail lst)))
))

(defun foldl (f z lst) (
	letrec (
		(aux (lambda (lst' acc) (
			if (empty? lst') acc
			(aux (tail lst') (f acc (head lst')))
		)))
	)
	aux lst z
))

(defun map (f lst) (
	letrec (
		(aux (lambda (lst') (
			if (empty? lst') lst'
			(cons (f (head lst')) (aux (tail lst')))
		)))
	)
	aux lst
))
