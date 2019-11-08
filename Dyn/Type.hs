module Dyn.Type where

import Debug.Trace
import qualified Data.List as L

import Dyn.AST
import Dyn.Classes
import Dyn.Map

-------------------------------------------------------------------------------

apply :: [Ifce] -> [Decl] -> [Decl]
apply x y = mapDecls (fD,fE,fPz) x cz [] y where

  -- apply Type expressions
  -- Type (1+1)  --> Type Nat
  fE :: [Ifce] -> Ctrs -> [Decl] -> Type -> Expr -> Expr
  fE _ _ dsigs _ (ECall z (ECons z1 ["Type"]) e2) = EType z $ toType dsigs e2
  fE _ _ _ _ e = e

  -- removes TAny decls (prevents double decl)
  fD :: [Ifce] -> Ctrs -> [Decl] -> Decl -> [Decl]
  fD _ _ dsigs d@(DSig _ _ _ TAny) = []
  fD _ _ dsigs d@(DSig _ _ _ _)    = [d]

  fD ifces ctrs dsigs datr@(DAtr z pat1 whe2@(ExpWhere (z2,ds2,e2))) =
    aux pat1 (toType dsigs whe2) ++ [datr] where

      aux _ TAny = []

      -- x :: ? = 10       --> x :: Nat = 10
      aux pat@(PWrite z id) tp2 = case (toType dsigs pat, tp2) of
        (TAny, tp2) -> [DSig z id cz tp2]               -- inferred from whe2
        (tp1,  tp2) | (isSup ifces ctrs tp1 tp2) -> []  -- TODO: check types
        (tp1,  tp2) -> error $ show $ (toString tp1, toString tp2)

      aux pat@(PTuple z ps) tp2 = case (toType dsigs pat, tp2) of
        (TTuple ts1, TTuple ts2) -> concatMap f $ zip ps ts2 where
                                      f (p,t2) = aux p t2

      aux pat _ = error $ toString pat

-------------------------------------------------------------------------------

isSup :: [Ifce] -> Ctrs -> Type -> Type -> Bool
isSup _ _ tp1 tp2 = isSup' tp1 tp2
--isSup ifces (Ctrs cs) tp1 (TVar "a") = error $ show (toString tp1, cs)
--isSup _     _         (TData hr1) (TData hr2) = hr1 `L.isPrefixOf` hr2
--isSup _ _ tp1 tp2 = error $ show (toString tp1, toString tp2)

isSup' :: Type -> Type -> Bool
isSup' tp1            (TVar "a")     = True
isSup' (TVar "a")     tp2            = True
isSup' (TData hr1 _)  (TData hr2 _)  = hr1 `L.isPrefixOf` hr2   -- TODO: _
--isSup' tp1 tp2 = True
isSup' tp1 tp2 = error $ show (toString tp1, toString tp2)
