module Dyn.Type where

import Debug.Trace
import Data.Maybe (fromJust)
import Data.List  (find)
import qualified Data.Map as M

import Dyn.AST

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

-- Env:   context of variables
-- Type:  expected type
-- Where: expression (+ decls) to evaluate
-- Where: transformed expression (+ decls) (maybe the same)
--
poly :: Env -> Type -> Where -> Where
poly _   (Type (_,TUnit,_))  w@(Where (_,EUnit _,_))    = w -- () vs () --> ()
poly _   (Type (_,TVar _,_)) w                          = w -- a  vs e  --> e
                                                            -- T  vs T  --> w
                                                            -- T  vs x::a --> ???
poly env xtp@(Type (_,TData xhr,_)) w@(Where (z1,EVar z2 id,ds)) =
  case (xtp,tp) of
    _ | (xtp==tp)             -> w
    (_, Type (_,TVar tid,cs)) -> Where (z1, EVar z2 id,ds++ds') where
                                  ds'  = traceShow dicts []
                                  ifcs = snd $ head $ filter ((==tid).fst) cs
                                  dicts = map (\ifc -> "d"++tid++ifc++concat xhr) ifcs
  where
    DSig _ _ tp = fromJust $ find f env where
                    f (DSig _ x _) = (id == x)

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
