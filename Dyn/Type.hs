module Dyn.Type where

import Debug.Trace

import Dyn.AST
import Dyn.Classes

-------------------------------------------------------------------------------

apply :: [Ifce] -> [Decl] -> [Decl]
apply x y = mapDecls (fD,fE,fPz) x cz [] y where

  -- apply Type expressions
  -- Type (1+1)  --> Type Nat
  fE :: [Ifce] -> Ctrs -> [Decl] -> Expr -> Expr
  fE _ _ dsigs (ECall z (ECons z1 ["Type"]) e2) = EType z $ toType dsigs e2
  fE _ _ _ e = e

  fD :: [Ifce] -> Ctrs -> [Decl] -> Decl -> [Decl]

  fD _ _ dsigs d@(DSig _ _ _ TAny) = []   -- removes itself (prevents double decl)
  fD _ _ dsigs d@(DSig _ _ _ _)    = [d]

  fD _ _ dsigs datr@(DAtr z pat1 whe2@(ExpWhere (z2,ds2,e2))) =
    aux pat1 (toType dsigs whe2) ++ [datr] where

      aux _ TAny = []

      -- x :: ? = 10       --> x :: Nat = 10
      aux pat@(PWrite z id) tp = case (toType dsigs pat, tp) of
        (TAny, tp)   -> [DSig z id cz tp]  -- inferred from whe2
        otherwise    -> [] -- TODO: check types

      aux pat@(PTuple z ps) tp = case (toType dsigs pat, tp) of
        (TTuple ts1, TTuple ts2) -> concatMap f $ zip ps ts2 where
                                      f (p,t2) = aux p t2

      aux pat _ = error $ toString pat
