module Dyn.Order where

import Debug.Trace
import qualified Data.List as L
import qualified Data.Set  as S

import Dyn.AST
import Dyn.Classes
import Dyn.Map

-------------------------------------------------------------------------------

apply :: Prog -> Prog
apply globs = mapGlobs (mS,mDz,mWz,mPz,mEz) globs where
  mS _ _ dsigs ds = map fst $ L.sortBy fsort $ zip ds (map getAccs ds)

  fsort :: (Decl, (S.Set ID_Var,S.Set ID_Var)) -> (Decl, (S.Set ID_Var,S.Set ID_Var)) -> Ordering
  fsort (_, (xs1,ys1)) (_, (xs2,ys2)) =
    if not $ null $ S.intersection xs1 ys2 then
        GT
    else if not $ null $ S.intersection ys1 xs2 then
        LT
    else
        EQ

  getAccs :: Decl -> (S.Set ID_Var,S.Set ID_Var)    -- xs <- ys
  getAccs decl = (xs,ys) where
    xs = collectDecl cPWrite decl
    ys = collectDecl cEVar   decl

  cEVar = CollectFs (cW,cPz,cE) where
            cE :: Expr -> S.Set ID_Var
            cE (EVar _ id) = S.singleton id
            cE _           = S.empty

            cW :: ExpWhere -> S.Set ID_Var -> S.Set ID_Var
            cW whe@(ExpWhere (_,ds,_)) es = S.difference es (S.unions $ map f ds)
              where
                f (DSig _ id _ _) = S.singleton id
                f _               = S.empty
