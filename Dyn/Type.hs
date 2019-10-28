module Dyn.Type where

import Debug.Trace
import Data.Maybe (fromJust)
import Data.List  (find)
import qualified Data.Map as M

import Dyn.AST
import Dyn.Ifce

type Env = [Decl]

-------------------------------------------------------------------------------

-- Env:   context of variables
-- TCtrs: context of constraints
-- Expr:  expression to get the type
-- Type:  returned expression type
getType :: Env -> TCtrs -> Expr -> Type
getType _   cs  (EUnit z)      = Type (z,  TUnit, cs)
getType env cs1 (EVar  z1 id1) = Type (z1, ttp2,  cs') where
  DSig _ id2 (Type (_,ttp2,cs2)) = fromJust $ find f env where
                                    f (DSig _ id2 _) = (id1 == id2)
  cs' = ctrsFromMap $ M.union (ctrsToMap cs1) (ctrsToMap cs2)

-------------------------------------------------------------------------------

{-
poly env xtp expr =

  if cs == cs' then
    False
  else
    True
  where
    Type (_, _, cs') = getType env cs expr
-}

-------------------------------------------------------------------------------

--run :: Where -> Where
--run (Where (_, expr, decls)) = poly (filter isDSig decls) expr
