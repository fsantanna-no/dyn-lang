module Dyn.Eval where

import Debug.Trace

import Dyn.AST

type Env = [(ID_Var,Expr)]

-------------------------------------------------------------------------------

evalExpr :: Env -> Expr -> Expr
evalExpr env (EUnit z)    = EUnit z
evalExpr env (EVar  z id) = snd $ head $ filter ((==id).fst) env

-------------------------------------------------------------------------------

evalDcl :: Env -> Dcl -> [(ID_Var, Expr)]
evalDcl env (Dcl (_, id, _, Just w))  = [(id, evalWhere env w)]
evalDcl env (Dcl (_, id, _, Nothing)) = []

-------------------------------------------------------------------------------

evalWhere :: Env -> Where -> Expr
evalWhere env (Where (_, e, dcls)) = evalExpr env' e where
                                      env' = foldr f env dcls where
                                              f :: Dcl -> Env -> Env
                                              f dcl env = evalDcl env dcl ++ env
