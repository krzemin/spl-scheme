module Sugar (desugar) where

import Expr

desugar :: Expr -> Expr

-- desugaring atom-bool constants
desugar (Atom "true") = Bool True
desugar (Atom "false") = Bool False

-- let desugared as lambda abstraction + application
desugar (List [Atom "let", Atom x, e0, e1]) =
  List [List [Atom "lambda", Atom x, desugar e1], desugar e0]

-- let* desugared as lambda abstractions + applications
desugar (List [Atom "let*", List [], e]) = e
desugar (List [Atom "let*", List (List [Atom x, e'] : bs), e]) =
  desugar (List [Atom "let", Atom x, e', desugar e''])
  where
    e'' = List [Atom "let*", List bs, e]

-- if desugared as primitive cond
desugar (List [Atom "if", b, e0, e1]) =
  List [Atom "cond", desugar b, desugar e0, desugar e1]

-- fst, snd, head, tail functions desugared as car, cdr
desugar (List [Atom "fst", e]) = List [Atom "car", desugar e]
desugar (List [Atom "head", e]) = List [Atom "car", desugar e]
desugar (List [Atom "snd", e]) = List [Atom "cdr", desugar e]
desugar (List [Atom "tail", e]) = List [Atom "cdr", desugar e]

-- nil desugared as equal
desugar (List [Atom "nil?", xs]) =
  List [Atom "equals?", List [Atom "quote", List []], desugar xs]

-- desugaring multi-argument lambdas
desugar (List [Atom "lambda", List [], e]) = desugar e
desugar (List [Atom "lambda", List (Atom x : xs), e]) =
  List [Atom "lambda", Atom x, desugar e']
  where
    e' = List [Atom "lambda", List xs, e]

-- desugaring function definition
desugar (List [Atom "defun", List (Atom f : args), e]) =
	List [Atom "define", Atom f, desugar lam]
	where
		lam = List [Atom "lambda", List args, e]

desugar (List xs) = List $ map desugar xs

desugar expr = expr

