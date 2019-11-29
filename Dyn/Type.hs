module Dyn.Type where

import Debug.Trace
import Data.Maybe (isNothing)
import qualified Data.List as L

import Dyn.AST
import Dyn.Classes
import Dyn.Map

-------------------------------------------------------------------------------

apply :: Prog -> Prog
apply globs =
  mapGlobs (mSz,mDz,mWz,mPz,mE1) $
  mapGlobs (mS,mDz,mWz,mPz,mE2)  $
  globs where

-------------------------------------------------------------------------------

  -- apply Type expressions
  -- Type (1+1)  --> Type Nat
  mE1 :: [Glob] -> CTs -> [Decl] -> Type -> Expr -> Expr
  mE1 _ _ dsigs _ (ECall z (ECons z1 ["Type"]) e2) = EType z $ toType dsigs e2
  mE1 _ _ _ _ e = e

-------------------------------------------------------------------------------

  mE2 :: [Glob] -> CTs -> [Decl] -> Type -> Expr -> Expr
  mE2 globs ctrs dsigs xtp (ECase z ee l) = ECase z ee (map f l) where

    f :: (Patt,ExpWhere) -> (Patt,ExpWhere)
    f (pat, ExpWhere (z,ds,e)) = (pat, ExpWhere (z, union ds ds', e)) where
                                  ds' :: [Decl]
                                  ds' = aux globs ctrs dsigs pat (toType dsigs ee)

  mE2 _ _ _ _ e = e

  mS :: [Glob] -> CTs -> [Decl] -> [Decl] -> [Decl]
  mS globs ctrs dsigs decls = union decls inferreds where

    -- infer type of pat1 (add dsig) based on type of whe2

    inferreds = concatMap f decls where
      f datr@(DAtr z pat1 whe2@(ExpWhere (z2,ds2,e2))) =
        aux globs ctrs dsigs pat1 (toType dsigs whe2) where

      f _ = []

-------------------------------------------------------------------------------

aux :: [Glob] -> CTs -> [Decl] -> Patt -> Type -> [Decl]
aux _ _ _ (PError _ _) _ = []
aux _ _ _ (PArg   _)   _ = []
aux _ _ _ (PAny   _)   _ = []
aux _ _ _ (PUnit  _)   _ = []
aux _ _ _ (PCons  _ _) _ = []
aux _ _ _ (PRead  _ _) _ = []

aux globs ctrs dsigs (PCall _ (PCons _ hr) pat) _ = aux globs ctrs dsigs pat (dataFindFullSt globs hr)

aux _ _ _ _ TAny = []

-- x :: ? = 10        --> x :: Nat = 10
aux globs ctrs dsigs pat@(PWrite z id) tp2 = case (toType dsigs pat, tp2) of
    (TAny, tp2) -> [DSig z id cz tp2]               -- inferred from whe2
    (tp1,  tp2) | (isSup globs ctrs tp1 tp2) -> []  -- TODO: check types
    (tp1,  tp2) -> error $ show $ (toString tp1, toString tp2)

-- (x,y) = (True,())  --> x::Bool, y::()
aux globs ctrs dsigs pat@(PTuple z ps) tp2 =
  case tp2 of
    TTuple ts2 -> concatMap f $ zip ps ts2 where
                    f (p,t2) = aux globs ctrs dsigs p t2
    otherwise  -> []

aux _ _ _ pat _ = error $ toString pat

-------------------------------------------------------------------------------

-- removes TAny decls that have been inferred
union decls inferreds = filter isAnyInferred decls ++ inferreds where
  isAnyInferred (DSig _ id _ TAny) =
    isNothing $ L.find (\(DSig _ x _ _) -> x==id) inferreds
  isAnyInferred _ = True

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
