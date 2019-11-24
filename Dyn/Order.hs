module Dyn.Order where

import Debug.Trace
import qualified Data.List as L
import qualified Data.Set  as S

import Dyn.AST
import Dyn.Classes
import Dyn.Map

-------------------------------------------------------------------------------

type Pair   = (S.Set ID_Var, S.Set ID_Var)
type Triple = (Decl, Pair)

apply :: Prog -> Prog
apply globs = mapGlobs (mS,mDz,mWz,mPz,mEz) globs where
  mS _ _ dsigs ds = map fst $ sort $ zip ds (map getAccs ds)

  getAccs :: Decl -> Pair    -- xs <- ys
  getAccs decl@(DAtr _ pat whe) = (xs,ys) where
    xs = collectPatt cPWrite pat
    ys = collectDecl cEVar   decl
  getAccs _ = (S.empty,S.empty)

  cEVar = CollectFs (cW,cPz,cE) where
            cE :: Expr -> S.Set ID_Var
            cE (EVar _ id) = S.singleton id
            cE _           = S.empty

            cW :: ExpWhere -> S.Set ID_Var -> S.Set ID_Var
            cW whe@(ExpWhere (_,ds,_)) es = S.difference es (S.unions $ map f ds)
              where
                f (DSig _ id _ _) = S.singleton id
                f _               = S.empty

sort :: [Triple] -> [Triple]
sort [] = []
sort l  = f1 l S.empty where

  f1 :: [Triple] -> S.Set ID_Var -> [Triple]
  f1 todos decls = case f2 todos decls of
                    --([],  noks)    -> error $ show (noks,todos,decls) --"TODO: cycle"
                    ([],  ok:noks) -> ok : f1 noks decls' where
                                        decls' = S.union decls ((fst.snd) ok)
                    (oks, [])      -> oks
                    (oks, noks)    -> f1 noks decls' ++ oks where
                                        decls' = S.union decls (S.unions $ map (fst.snd) oks)

  -- > todos: list of pending pairs declarations to sort
  -- > decls: set of already declared vars
  -- < : pairs that are ok (all used variables are in decls) // pairs that are not ok yet
  f2 :: [Triple] -> S.Set ID_Var -> ([Triple],[Triple])
  f2 todos decls = (filter allDecl todos, filter (not.allDecl) todos) where
                      allDecl (_,(me,uses)) = S.null $ S.difference uses (S.union me decls)   -- all uses are declared
