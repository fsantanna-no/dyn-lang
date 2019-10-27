module Dyn.Type where

import Debug.Trace
import Data.Maybe (fromJust)
import Data.List  (find)

import Dyn.AST

-------------------------------------------------------------------------------

type Env = [Decl]

--declsToEnv :: [Decls] -> Env
--declsToEnv decls = (filter isDSig decls)

-------------------------------------------------------------------------------

getType :: Env -> TCtrs -> Expr -> Type
getType _   cs  (EUnit z)      = Type (z,  TUnit, cs)
getType env cs1 (EVar  z1 id1) = Type (z1, ttp2,  cs') where
  DSig _ id2 (Type (_,ttp2,cs2)) = fromJust $ find f env where
                                    f (DSig _ id2 _) = (id1 == id2)
  cs' = case cs1 == cs2 of
          True  -> cs1
          False -> error "TODO: match cs"

-------------------------------------------------------------------------------

poly :: Env -> Expr -> ()
poly env evar@(EVar _ id) = traceShow tp' () where
  tp' = getType env cz evar

-------------------------------------------------------------------------------

--run :: Where -> Where
--run (Where (_, expr, decls)) = poly (filter isDSig decls) expr
