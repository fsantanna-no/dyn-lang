module Dyn.Type where

import Debug.Trace
import Data.Maybe (isNothing)
import qualified Data.List as L

import Dyn.AST
import Dyn.Classes
import Dyn.Map

-------------------------------------------------------------------------------

apply :: Prog -> Prog -> Prog
apply origs globs = mapGlobs (mS,mDz,mWz,mPz,mE) origs globs where

  -- apply Type expressions
  -- Type (1+1)  --> Type Nat
  mE :: [Glob] -> CTs -> [Decl] -> Type -> Expr -> Expr
  mE _ _ dsigs _ (ECall z (ECons z1 ["Type"]) e2) = EType z $ toType dsigs e2
  mE _ _ _ _ e = e

  mS :: [Glob] -> CTs -> [Decl] -> [Decl] -> [Decl]
  mS globs cts dsigs decls = dsigs' ++ inferreds' where

    -- removes TAny decls that have been inferred

    dsigs' = filter isAnyInferred decls where
              isAnyInferred (DSig _ id _ TAny) =
                isNothing $ L.find (\(DSig _ x _ _) -> x==id) inferreds'
              isAnyInferred _ = True

    -- infer type of pat1 (add dsig) based on type of whe2

    inferreds' = concatMap f decls where
      f datr@(DAtr z pat1 whe2@(ExpWhere (z2,ds2,e2))) =
        aux pat1 (toType dsigs whe2) where
          aux _                 TAny = []

          -- x :: ? = 10       --> x :: Nat = 10
          aux pat@(PWrite z id) tp2  = case (toType dsigs pat, tp2) of
              (TAny, tp2) -> [DSig z id cz tp2]               -- inferred from whe2
              (tp1,  tp2) | (isSup globs cts tp1 tp2) -> []  -- TODO: check types
              (tp1,  tp2) -> error $ show $ (toString tp1, toString tp2)

          aux pat@(PTuple z ps) tp2  = case (toType dsigs pat, tp2) of
              (TTuple ts1, TTuple ts2) -> concatMap f $ zip ps ts2 where
                                            f (p,t2) = aux p t2

          aux pat _ = error $ toString pat

      f _ = []

-------------------------------------------------------------------------------

isSup :: [Glob] -> CTs -> Type -> Type -> Bool
isSup _ _ tp1 tp2 = isSup' tp1 tp2
--isSup ifces (Ctrs cs) tp1 (TVar "a") = error $ show (toString tp1, cs)
--isSup _     _         (TData hr1) (TData hr2) = hr1 `L.isPrefixOf` hr2
--isSup _ _ tp1 tp2 = error $ show (toString tp1, toString tp2)

isSup' :: Type -> Type -> Bool
isSup' tp1               (TVar "a")        = True
isSup' (TVar "a")        tp2               = True
isSup' (TData hr1 _)     (TData hr2 _)     = hr1 `L.isPrefixOf` hr2   -- TODO: _
isSup' (TFunc inp1 out1) (TFunc inp2 out2) = (inp1 == inp2) && (out1 == out2) -- TODO: covar/contravar
--isSup' tp1 tp2 = True
isSup' tp1 tp2 = error $ show (toString tp1, toString tp2)
