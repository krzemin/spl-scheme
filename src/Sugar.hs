module Sugar (desugar) where

import Expr

desugar :: Expr -> Expr

-- let desugared as lambda abstraction + application
desugar (List [Atom "let", Atom x, e0, e1]) =
  List [List [Atom "lambda", Atom x, e1], e0]

desugar expr = expr

