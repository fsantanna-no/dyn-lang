module Dyn.CC where

import Debug.Trace

import Dyn.AST
import Dyn.Parser

-------------------------------------------------------------------------------

ccExpr :: String -> Expr -> String
ccExpr _  (EError (ln,_) msg) = "error_at_line(0,0,NULL," ++ show ln ++ ",\"%s\"," ++
                                  "\"" ++ msg ++ "\");\n"
ccExpr to (EVar   _ id) = to ++ " = " ++ id ++ ";\n"
ccExpr to (EUnit  _)    = ""

ccExpr to (ECase (ln,_) e cs) = e' ++ cs' where
  e'  = ccExpr "__dyn_e" e
  cs' = "if (0) {}\n" ++
        concatMap f cs ++
        "else { error_at_line(0,0,NULL," ++ show ln ++ ", \"non-exhaustive patterns\"); }\n"
        where
          f :: (Patt, ExpWhere) -> String
          f (pat,whe) = "else if (" ++ match pat "__dyn_e" ++ ") { " ++ ccExpWhere to whe ++ "; }\n"

ccExpr _ x = error $ show x
{-
evalExpr env (EArg   z)     = envRead env z "..."
evalExpr env (ECons  z hr)  = EData z hr (EUnit z)

evalExpr env (EFunc  z tp cs ups bdy) = EFunc z tp cs (map f ups) bdy where
  f (id,_) = (id, evalExpr env (EVar z id))

evalExpr env (ETuple z es) = toError $ map (evalExpr env) es
  where
    toError es = case find isEError es of
      Nothing  -> ETuple z es
      Just err -> err

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
-}

-------------------------------------------------------------------------------

-- Returns:
--  Left  e   -- error e
--  Right b   -- match ok or not

type Match = Either Expr Bool

--       pat     e       ret
match :: Patt -> String -> String

match (PAny   _)    _  = "true"
match (PUnit  _)    _  = "true"
match (PArg   z)    e  = "(_dyn_arg = " ++ e ++ " , true)"
match (PWrite z id) e  = "(" ++ id ++ " = " ++ e ++ " , true)"

{-
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
-}

-------------------------------------------------------------------------------

ccDecl :: Decl -> String
ccDecl (DAtr _ p w)   = ccExpWhere "_dyn_atr" w ++ match p "_dyn_atr" ++ ";\n" where
ccDecl (DSig _ _ _ _) = error "TODO"

-------------------------------------------------------------------------------

ccExpWhere :: String -> ExpWhere -> String
ccExpWhere to (ExpWhere (_, dcls, e)) = concatMap ccDecl dcls ++ ccExpr to e

-------------------------------------------------------------------------------

ccProg :: Prog -> String
ccProg prog = e' ++ "print(_dyn_p);\n" where
    e' = ccExpWhere "_dyn_p" $ ExpWhere (pz, map globToDecl $ filter isGDecl prog, EVar pz "main")

{-
evalString :: String -> String
evalString input = evalStringF Prelude.id input

evalStringF :: (Prog->Prog) -> String -> String
evalStringF f input =
  case parse input of
    Left  err  -> err
    Right prog -> toString $ evalProg $ f prog
-}
