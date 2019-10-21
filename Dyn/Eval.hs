module Dyn.Eval where

import Debug.Trace
import Data.Bool    (bool)
import Data.List    (find)

import Dyn.AST
import Dyn.Parser

-------------------------------------------------------------------------------

type Env = [(ID_Var,Expr)]

envRead :: Env -> Ann -> ID_Var -> Expr
envRead env z id =
  case filter ((==id).fst) env of
    []        -> EError z $ "unassigned variable '" ++ id ++ "'"
    ((_,x):_) -> x

envWrite :: Env -> ID_Var -> Expr -> Env
envWrite env id e = (id,e) : env

-------------------------------------------------------------------------------

evalExpr :: Env -> Expr -> Expr
evalExpr env (EError z msg) = EError z msg
evalExpr env (EUnit  z)     = EUnit  z
evalExpr env (EVar   z id)  = envRead env z id
evalExpr env (EArg   z)     = envRead env z "..."
evalExpr env (ECons  z hr)  = EData z hr (EUnit z)

evalExpr env (ETuple z es) = toError $ map (evalExpr env) es
  where
    toError es = case find isEError es of
      Nothing  -> ETuple z es
      Just err -> err

evalExpr env (ECase z e cs) =
  case filter f $ map g cs of
    []                  -> EError z "pattern match fail"
    ((Left e,     _):_) -> e
    ((Right True, e):_) -> e
  where
    e' = evalExpr env e

    g :: (Patt, Where) -> (Either Expr Bool, Expr)
    g (pat,whe) = (ret, evalWhere env' whe) where
                    (env',ret) = match env pat e'

    f :: (Either Expr Bool, Expr) -> Bool
    f (Right False, _) = False  -- skip unmatched
    f _                = True   -- keep matched/errros

evalExpr env (ECall _ (EError z msg)  _)   = EError z msg
evalExpr env (ECall z f arg) =
  case (evalExpr env f, evalExpr env arg) of
    ((EData _ ["Show"] _) , x              ) -> traceShowId x
    (e1@(EError _ _)      , _              ) -> e1
    (_                    , e2@(EError _ _)) -> e2
    ((EFunc _ _ f')       , arg'           ) -> evalWhere env' f' where
                                                  env' = envWrite env "..." arg'
    ((EData z1 hr _)      , arg'           ) -> EData z1 hr arg'
    (_                    , _              ) -> EError z "invalid call"

evalExpr _ v = v

-------------------------------------------------------------------------------

-- Returns:
--  Left  e   -- error e
--  Right b   -- match ok or not

type Match = (Env, Either Expr Bool)

--       env    pat     e       ret
match :: Env -> Patt -> Expr -> Match

match env (PError z msg) _             = (env, Left $ EError z msg)
match env (PAny   _)     _             = (env, Right True)
match env _             (EError z msg) = (env, Left $ EError z msg)
match env (PUnit  _)    (EUnit _)      = (env, Right True)

match env (PWrite z id) e  = (envWrite env id e, Right True)
match env (PRead  z e1) e2 = match env (toPatt $ evalExpr env e1) e2 where
                              toPatt :: Expr -> Patt
                              toPatt (EUnit  z)       = PUnit  z
                              toPatt (EData  z hr st) = PCall  z (PCons z hr) (toPatt st)
                              toPatt (ETuple z es)    = PTuple z (map toPatt es)
                              toPatt _ = PError z "invalid pattern"

match env (PTuple _ ps) (ETuple _ es) = foldr f (env, Right True) (zip ps es)
  where
    f (pat,e) (env, Right True) = match env pat e
    f _       (env, ret)        = (env, ret)

match env (PCons _ hrp) (EData _ hre (EUnit _)) = (env, Right $ hrp == hre)

match env (PCall _ (PCons _ hrp) pat) (EData _ hre e) =
  if hrp == hre then
    match env pat e
  else
    (env, Right False)

--match env pat e = traceShow (pat,e) (env, Right False)
match env _ _ = (env, Right False)

-------------------------------------------------------------------------------

evalDcl :: Env -> Dcl -> Match
evalDcl env (Dcl (_, p, _, Just w)) = match env p (evalWhere env w)
evalDcl env _ = (env, Right True)

-------------------------------------------------------------------------------

evalWhere :: Env -> Where -> Expr
evalWhere env (Where (z, e, dcls)) =
  case foldr f (env, Right True) dcls of
    (env', Right True)  -> evalExpr env' e
    (_,    Right False) -> EError z "invalid assignment"
    (_,    Left  err)   -> err
  where
    f :: Dcl -> Match -> Match
    f dcl (env, Right True)  = evalDcl env dcl
    f dcl (env, Right False) = (env, Left $ EError z "invalid assignment")
    f dcl (env, Left  e)     = (env, Left e)           -- found error

-------------------------------------------------------------------------------

evalProg :: Prog -> Expr
evalProg w = evalWhere [] w

run :: String -> String
run input =
  case parse input of
    Left  err -> err
    Right exp -> exprToString 0 $ evalProg exp
