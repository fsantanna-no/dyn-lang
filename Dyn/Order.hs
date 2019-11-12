module Dyn.Order where

import Debug.Trace
import qualified Data.List as L
import qualified Data.Set  as S

import Dyn.AST
import Dyn.Classes
import Dyn.Map

-------------------------------------------------------------------------------

apply :: Prog -> Prog -> Prog
apply origs globs = mapGlobs (fS,mDz,mWz,mEz,mPz) origs globs where
  fS _ _ dsigs ds = ds --map fst $ L.sortBy fsort $ zip ds (map getAccs ds)

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
    xs = collectDecl pWrites decl
    ys = collectDecl eVars   decl

  fz = const S.empty
  fWs _ x = x

  pWrites = CollectFs (fWs,fP,fz) where
              fP :: Patt -> S.Set ID_Var
              fP (PWrite _ id) = S.singleton id
              fP _             = S.empty
  eVars   = CollectFs (fW,fz,fE) where
              fE :: Expr -> S.Set ID_Var
              fE (EVar _ id) = S.singleton id
              fE _           = S.empty

              fW :: ExpWhere -> S.Set ID_Var -> S.Set ID_Var
              fW whe@(ExpWhere (_,ds,_)) es = S.difference es (S.unions $ map f ds)
                where
                  f (DSig _ id _ _) = S.singleton id
                  f _               = S.empty

-------------------------------------------------------------------------------

newtype CollectFs a = CollectFs (ExpWhere->S.Set a->S.Set a, Patt->S.Set a, Expr->S.Set a)

collectGlobs :: Ord a => CollectFs a -> Prog -> S.Set a
collectGlobs fs globs = collectDecls fs $ map globToDecl globs

collectDecls :: Ord a => CollectFs a -> [Decl] -> S.Set a
collectDecls fs decls = S.unions $ map (collectDecl fs) decls

collectDecl :: Ord a => CollectFs a -> Decl -> S.Set a
collectDecl fs (DSig _ _ _ _)   = S.empty
collectDecl fs (DAtr _ pat whe) = S.union (collectPatt fs pat) (collectWhere fs whe)

collectWhere :: Ord a => CollectFs a -> ExpWhere -> S.Set a
collectWhere fs@(CollectFs (fW,_,_)) w@(ExpWhere (_,ds,e)) = fW w $ S.union (collectDecls fs ds) (collectExpr fs e)

collectPatt :: Ord a => CollectFs a -> Patt -> S.Set a
collectPatt fs@(CollectFs (_,fP,_)) p = S.union (fP p) (rec p) where
  rec (PRead  _ e)     = collectExpr fs e
  rec (PTuple _ ps)    = S.unions $ map (collectPatt fs) ps
  rec (PCall  _ p1 p2) = S.union (collectPatt fs p1) (collectPatt fs p2)
  rec _                = S.empty

collectExpr :: Ord a => CollectFs a -> Expr -> S.Set a
collectExpr fs@(CollectFs (_,_,fE)) e = S.union (fE e) (rec e) where
  rec (ETuple _ es)        = S.unions $ map (collectExpr fs) es
  rec (EFunc  _ _ _ _ whe) = collectWhere fs whe
  rec (ECall  _ e1 e2)     = S.union (collectExpr fs e1) (collectExpr fs e2)
  rec (ECase  _ e  l)      = S.union (collectExpr fs e) (S.unions $ map f l) where
                              f (pat,whe) = S.union (collectPatt fs pat) (collectWhere fs whe)
  rec (EData  _ _ e)       = collectExpr fs e
  rec _                    = S.empty
