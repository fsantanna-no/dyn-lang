module Dyn.Eval where

import Debug.Trace
import Data.Bool    (bool)
import Data.List    (find, isPrefixOf, length)

import Dyn.AST
import Dyn.Parser

-------------------------------------------------------------------------------

type Env = [(ID_Var,Expr)]

envRead :: Env -> Pos -> ID_Var -> Expr
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

evalExpr env (EFunc  z tp cs ups bdy) = EFunc z tp cs (map f ups) bdy where
  f (id,_) = (id, evalExpr env (EVar z id))

evalExpr env (ETuple z es) = toError $ map (evalExpr env) es
  where
    toError es = case find isEError es of
      Nothing  -> ETuple z es
      Just err -> err

evalExpr env (ECase z e cs) =
  case filter f $ map g cs of
    []                  -> EError z "non-exhaustive patterns"
    ((Left e,     _):_) -> e
    ((Right True, e):_) -> e
  where
    e' = evalExpr env e

    g :: (Patt, ExpWhere) -> (Either Expr Bool, Expr)
    g (pat,whe) = (ret, evalExpWhere env' whe) where
                    (env',ret) = match env pat e'

    f :: (Either Expr Bool, Expr) -> Bool
    f (Right False, _) = False  -- skip unmatched
    f _                = True   -- keep matched/errros

evalExpr env (ECall _ (EError z msg)  _)   = EError z msg
evalExpr env (ECall z f arg) =
  case (evalExpr env f, evalExpr env arg) of
    ((EData _ ["Show"] _) , x              ) -> traceShow (toString x) x
    (e1@(EError _ _)      , _              ) -> e1
    (_                    , e2@(EError _ _)) -> e2
    ((EData z1 hr _)      , arg'           ) -> EData z1 hr arg'
    ((EFunc _ _ _ ups f') , arg'           ) -> evalExpWhere env' f' where
      env' = envWrite (ups++env) "..." arg'
    --(x,y) -> error $ show (x,y)

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

match env (PArg   z)    e  = (envWrite env "..." e, Right True)
match env (PWrite z id) e  = (envWrite env id    e, Right True)
match env (PRead  z e1) e2 = match env (toPatt $ evalExpr env e1) e2 where
                              toPatt :: Expr -> Patt
                              toPatt (EUnit  z)       = PUnit  z
                              toPatt (EData  z hr st) = PCall  z (PCons z hr) (toPatt st)
                              toPatt (ETuple z es)    = PTuple z (map toPatt es)
                              toPatt x = PError z $ "invalid pattern : " ++ show (toString x)

match env (PTuple _ ps) (ETuple _ es) = foldr f (env, Right True) (zip ps es)
  where
    f (pat,e) (env, Right True) = match env pat e
    f _       (env, ret)        = (env, ret)

match env (PCons _ hrp) (EData _ hre (EUnit _)) = (env, Right $ hrp `isPrefixOf` hre)

match env (PCall _ (PCons _ hrp) pat) (EData _ hre e) =
  if hrp `isPrefixOf` hre then
    match env pat $ f (length hre - length hrp) e
  else
    (env, Right False)
  where
    f 0 e = e
    f n (ETuple z [_,sup]) = f (n-1) sup    -- sup payload comes last
    f n e = e

--match env pat e = traceShow (pat,e) (env, Right False)
match env _ _ = (env, Right False)

-------------------------------------------------------------------------------

evalDecl :: Env -> Decl -> Match
evalDecl env (DAtr _ p w) = match env p (evalExpWhere env w)
evalDecl env _            = (env, Right True)

-------------------------------------------------------------------------------

evalExpWhere :: Env -> ExpWhere -> Expr
evalExpWhere env (ExpWhere (z, dcls, e)) =
  case foldr f (env, Right True) dcls of
    (env', Right True)  -> evalExpr env' e
    (_,    Right False) -> EError (getPos $ head dcls) "invalid assignment"
    (_,    Left  err)   -> err
  where
    f :: Decl -> Match -> Match
    f dcl (env, Right True)  = evalDecl env dcl
    f dcl (env, Right False) = (env, Left $ EError (getPos dcl) "invalid assignment")
    f dcl (env, Left  e)     = (env, Left e)           -- found error

-------------------------------------------------------------------------------

evalProg :: Prog -> Expr
evalProg prog =
  evalExpWhere [] $ ExpWhere (pz, map globToDecl $ filter isGDecl prog, EVar pz "main")

evalString :: String -> String
evalString input = evalStringF Prelude.id input

evalStringF :: (Prog->Prog) -> String -> String
evalStringF f input =
  case parse input of
    Left  err  -> err
    Right prog -> toString $ evalProg $ f prog
