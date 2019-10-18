module Dyn.Eval where

import Dyn.AST

-------------------------------------------------------------------------------

evalExpr :: Expr -> Expr
evalExpr (EUnit z) = EUnit z
