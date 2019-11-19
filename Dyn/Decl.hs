module Dyn.Decl where

import Debug.Trace
import qualified Data.List as L
import qualified Data.Set  as S

import Dyn.AST
import Dyn.Classes
import Dyn.Map

-- declare undeclared variables

apply :: Prog -> Prog
apply globs = mapGlobs (mS,mDz,mWz,mPz,mE) globs where
  mE _ _ _ _ (ECase z e l) = ECase z e $ map f l where
    f (pat, ExpWhere (z,ds,e)) = (pat, ExpWhere (z,ds++ds',e)) where

      ds' :: [Decl]
      ds' = map f $ S.toList idsDif where
              f :: ID_Var -> Decl
              f id = DSig z id cz TAny

      idsDif :: S.Set ID_Var
      idsDif = S.difference idsPat idsDs
      idsPat = collectPatt cPWrite pat
      idsDs  = S.unions $ map f ds where
                f (DSig _ id _ _) = S.singleton id
                f _               = S.empty

  mE _ _ _ _ e = e

  mS _ _ _ ds = ds ++ ds' where

    ds' :: [Decl]
    ds' = map f $ S.toList idsDif where
            f :: ID_Var -> Decl
            f id = DSig pz id cz TAny -- TODO: pz

    idsDif :: S.Set ID_Var
    idsDif = S.difference idsDAtr idsDSig

    idsDAtr = S.unions $ map f ds where
                f (DAtr _ pat _) = collectPatt cPWrite pat
                f _              = S.empty

    idsDSig = S.unions $ map f ds where
                f (DSig _ id _ _) = S.singleton id
                f _               = S.empty

