module Dyn.Eval where

import Debug.Trace
import Data.Bool    (bool)

import Dyn.AST

type Env = [(ID_Var,Expr)]

-------------------------------------------------------------------------------

evalExpr :: Env -> Expr -> Expr
evalExpr env (EError z)    = EError z
evalExpr env (EUnit  z)    = EUnit  z
evalExpr env (EVar   z id) = snd $ head $ filter ((==id).fst) env

evalExpr env (EIf z e p t f) = evalExpr env (bool f t $ match env p e') where
                                e' = evalExpr env e

-------------------------------------------------------------------------------

match :: Env -> Expr -> Expr -> Bool
match _ (EUnit _) (EUnit _) = True
match _ _         _         = False

-------------------------------------------------------------------------------

evalDcl :: Env -> Dcl -> [(ID_Var, Expr)]
evalDcl env (Dcl (_, EVar _ id, _, Just w))  = [(id, evalWhere env w)]
evalDcl env (Dcl (_, _, _, Nothing)) = []

-------------------------------------------------------------------------------

evalWhere :: Env -> Where -> Expr
evalWhere env (Where (_, e, dcls)) = evalExpr env' e where
                                      env' = foldr f env dcls where
                                              f :: Dcl -> Env -> Env
                                              f dcl env = evalDcl env dcl ++ env

-------------------------------------------------------------------------------

evalProg :: Where -> Expr
evalProg w = evalWhere [] w
