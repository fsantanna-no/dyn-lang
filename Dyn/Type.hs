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

-- Env:   context of variables
-- Type:  expected type
-- Where: expression (+ decls) to evaluate
-- Where: transformed expression (+ decls) (maybe the same)
--
poly :: [Ifce] -> Env -> Type -> Where -> Where
poly _ _ (Type (_,TUnit,_))  w@(Where (_,EUnit _,_)) = w -- () vs () --> ()
poly _ _ (Type (_,TVar _,_)) w                       = w -- a  vs e  --> e
                                                         -- T  vs T  --> w
                                                         -- T  vs x::a --> ???
poly ifces env xtp@(Type (_,TData xhr,_)) w@(Where (z1,EVar z2 id,ds)) =
  case (xtp,tp) of
    _ | (xtp==tp)             -> w

    (_, Type (_,TVar tid,cs)) -> Where (z1, EVar z2 id,ds++ds') where

      -- [("IEq",...)]
      ifc_ids = snd $ fromJust $ find ((==tid).fst) cs

      -- [Dict.IEq (eq,neq) = daIEqBool, ...]
      ds' :: [Decl]
      ds' = map f $
              -- [("IEq", "daIEqBool", (eq,neq)),...]
              zip3 ifc_ids dicts dclss where
                -- ["daIEqBool",...]
                dicts = map (\ifc -> "d"++tid++ifc++concat xhr) ifc_ids
                -- [(eq,neq),...]
                dclss = map ifceToIds $ map (findIfce ifces) ifc_ids
      f (ifc,dict,dcls) =
        DAtr z1 (PCall z1 (PCons z1 ["Dict",ifc]) (fromList $ map (PWrite z1) dcls))
                (Where (z1, EVar z1 dict, []))

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
