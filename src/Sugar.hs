module Sugar (desugar) where

import Expr

desugar :: Expr -> Expr

-- desugaring atom-bool constants
desugar (Atom "true") = Bool True
desugar (Atom "false") = Bool False

-- let desugared as lambda abstraction + application
desugar (List [Atom "let", Atom x, e0, e1]) =
  List [List [Atom "lambda", Atom x, desugar e1], desugar e0]

-- desugaring multi-argument lambdas
desugar (List [Atom "lambda", List [Atom x], e]) =
  List [Atom "lambda", Atom x, desugar e]
desugar (List [Atom "lambda", List (Atom x : xs), e]) =
  List [Atom "lambda", Atom x, desugar e']
  where
    e' = List [Atom "lambda", List xs, e]

desugar (List xs) = List $ map desugar xs

desugar expr = expr

