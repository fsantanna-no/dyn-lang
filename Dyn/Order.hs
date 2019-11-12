module Dyn.Order where

import Debug.Trace
import qualified Data.List as L

import Dyn.AST
import Dyn.Classes
import Dyn.Map

-------------------------------------------------------------------------------

apply :: Prog -> Prog -> Prog
apply origs globs = mapGlobs (fS,fDz,fEz,fPz) origs globs where
  fS _ _ dsigs ds = map fst $ L.sortBy fsort $ map addAccs ds
  fsort (DSig _ _ _ _,[]) _ = GT
  fsort _ (DSig _ _ _ _,[]) = LT

  fsort (DAtr _ (PWrite _ id1) _, ids1)
        (DAtr _ (PWrite _ id2) _, ids2) = if elem id1 ids2 then
                                            GT
                                          else if elem id2 ids1 then
                                            LT
                                          else
                                            GT

  addAccs d@(DSig _ _ _ _)   = (d, [])
  addAccs d@(DAtr _ pat whe) = traceShow (toString pat, accs) (d, accs) where
    accs = map (\(EVar _ id) -> id) $ toList e'
    ExpWhere (_,ds',e') = mapWhere (fSz,fDz,fE,fPz) globs cz [] TUnit whe
    fE _ _ _ _ e = fromList $ f e where
      f   (EUnit _)       = []
      f   (ECons _ _)     = []
      f e@(EVar  _ _)     = [e]
      f e@(ECall _ e1 e2) = f e1 ++ f e2
