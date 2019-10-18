module Dyn.Eval where

import Dyn.AST

-------------------------------------------------------------------------------

evalExpr :: Expr -> Expr
evalExpr (EUnit z) = EUnit z

-------------------------------------------------------------------------------

evalDcl :: Dcl -> Expr
evalDcl (Dcl (_, id, _, w)) = evalWhere w

-------------------------------------------------------------------------------

evalWhere :: Where -> Expr
evalWhere (Where (_, e, [])) = evalExpr e
