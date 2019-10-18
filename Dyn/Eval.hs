module Dyn.Eval where

import Dyn.AST

type Env = [(ID_Var,Expr)]

-------------------------------------------------------------------------------

evalExpr :: Env -> Expr -> Expr
evalExpr env (EUnit z) = EUnit z

-------------------------------------------------------------------------------

evalDcl :: Dcl -> (ID_Var, Expr)
evalDcl (Dcl (_, id, _, w)) = (id, evalWhere w)

-------------------------------------------------------------------------------

evalWhere :: Where -> Expr
evalWhere (Where (_, e, dcls)) = evalExpr env e where
                                  env = map evalDcl dcls
