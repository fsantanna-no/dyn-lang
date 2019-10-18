module Dyn.Eval where

import Debug.Trace
import Data.Bool    (bool)

import Dyn.AST

-------------------------------------------------------------------------------

type Env = [(ID_Var,Expr)]

envRead :: Env -> ID_Var -> Expr
envRead env id = snd $ head $ filter ((==id).fst) env

envWrite :: Env -> ID_Var -> Expr -> Env
envWrite env id e = (id,e) : env

-------------------------------------------------------------------------------

evalExpr :: Env -> Expr -> Expr
evalExpr env (EError z)    = EError z
evalExpr env (EUnit  z)    = EUnit  z
evalExpr env (EVar   z id) = envRead env id

evalExpr env (EIf z e p t f) =
  case snd $ match env False p e' of  -- snd: never changes env
    Left  err -> err -- error on match
    Right ok  -> evalExpr env $ bool f t ok
  where
    e'  = evalExpr env e

-------------------------------------------------------------------------------

-- Returns:
--  Left  e   -- error e
--  Right b   -- match ok or not

type Match = (Env, Either Expr Bool)

--       env    set     pat     e       ret
match :: Env -> Bool -> Expr -> Expr -> Match

match env _ _ (EError z) = (env, Left $ EError z)

match env True    (EVar z id) e = (envWrite env id e, Right True)
match env False p@(EVar z id) e = match env False (evalExpr env p) e

match env _ (EUnit  _) (EUnit _) = (env, Right True)

match env _ _ _ = (env, Right False)

-------------------------------------------------------------------------------

evalDcl :: Env -> Dcl -> Match
evalDcl env (Dcl (_, p, _, Just w)) = match env True p (evalWhere env w)
evalDcl env _ = (env, Right True)

-------------------------------------------------------------------------------

evalWhere :: Env -> Where -> Expr
evalWhere env (Where (_, e, dcls)) =
  case foldr f (env, Right True) dcls of
    (env', Right True) -> evalExpr env' e
    (_,    Left  err)  -> err
  where
    f :: Dcl -> Match -> Match
    f dcl (env, Right True)  = evalDcl env dcl
    f dcl (env, Right False) = (env, Left $ EError az) -- match fail (irrefutable pattern)
    f dcl (env, Left  e)     = (env, Left e)           -- found error

-------------------------------------------------------------------------------

evalProg :: Where -> Expr
evalProg w = evalWhere [] w
