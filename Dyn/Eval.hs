module Dyn.Eval where

import Debug.Trace
import Data.Bool    (bool)
import Data.List    (find)

import Dyn.AST
import Dyn.Parser

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
evalExpr env (EArg   z)    = envRead env "..."
evalExpr env (ECons  z hr) = EData z hr (EUnit z)

evalExpr env (ETuple z es) = toError $ map (evalExpr env) es
  where
    toError es = case find isEError es of
      Nothing  -> ETuple z es
      Just err -> err

evalExpr env (EIf z e p t f) = toError $ snd $ match env False p $ evalExpr env e
  where
    toError (Left err) = err
    toError (Right ok) = evalWhere env $ bool f t ok

evalExpr env (ECall _ (EError z)     _)   = EError z
evalExpr env (ECall z f arg) =
  case (evalExpr env f, evalExpr env arg) of
    ((EError z1)     , _          ) -> EError z1
    (_               , (EError z2)) -> EError z2
    ((EFunc _ _ f')  , arg'       ) -> evalWhere env' f' where
                                        env' = envWrite env "..." arg'
    ((EData z1 hr _) , arg'       ) -> EData z1 hr arg'
    (_               , _          ) -> EError z

evalExpr _ v = v

-------------------------------------------------------------------------------

-- Returns:
--  Left  e   -- error e
--  Right b   -- match ok or not

type Match = (Env, Either Expr Bool)

--       env    set     pat     e       ret
match :: Env -> Bool -> Expr -> Expr -> Match

match env _ _ (EError z) = (env, Left $ EError z)

match env _ (EUnit  _) (EUnit _) = (env, Right True)

match env True    (EVar z id) e = (envWrite env id e, Right True)
match env False p@(EVar z id) e = match env False (evalExpr env p) e

match env set (ETuple _ ps) (ETuple _ es) = foldr f (env, Right True) (zip ps es)
  where
    f (pat,e) (env, Right True) = match env set pat e
    f _       (env, ret)        = (env, ret)

match env set (ECall _ (ECons _ hrp) pat) (EData _ hre e) =
  if hrp == hre then
    match env set pat e
  else
    (env, Right False)

--match env set pat e = error $ show (pat,e)
match env _ _ _ = (env, Right False)

-------------------------------------------------------------------------------

evalDcl :: Env -> Dcl -> Match
evalDcl env (Dcl (_, p, _, Just w)) = match env True p (evalWhere env w)
evalDcl env _ = (env, Right True)

-------------------------------------------------------------------------------

evalWhere :: Env -> Where -> Expr
evalWhere env (Where (_, e, dcls)) =
  case foldr f (env, Right True) dcls of
    (env', Right True)  -> evalExpr env' e
    (_,    Right False) -> EError az
    (_,    Left  err)   -> err
  where
    f :: Dcl -> Match -> Match
    f dcl (env, Right True)  = evalDcl env dcl
    f dcl (env, Right False) = (env, Left $ EError az) -- match fail (irrefutable pattern)
    f dcl (env, Left  e)     = (env, Left e)           -- found error

-------------------------------------------------------------------------------

evalProg :: Prog -> Expr
evalProg w = evalWhere [] w

run :: String -> String
run input =
  case parse input of
    Left  err -> err
    Right exp -> exprToString $ evalProg exp
