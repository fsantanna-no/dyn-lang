module Dyn.Map where

import Debug.Trace
import Data.Bool    (bool)
import qualified Data.Set  as S

import Dyn.AST
import Dyn.Classes

-------------------------------------------------------------------------------

type CTs = [(Ctrs,Type)]

type MapFs = ( ([Glob]->CTs->[Decl]->[Decl]->[Decl]),
               ([Glob]->CTs->[Decl]->Decl->Decl),
               ([Glob]->CTs->[Decl]->Type->ExpWhere->ExpWhere),
               ([Glob]->CTs->[Decl]->Patt->Patt),
               ([Glob]->CTs->[Decl]->Type->Expr->Expr))

mSz _ _ _   ds = ds
mDz _ _ _   d  = d
mWz _ _ _ _ w  = w
mPz _ _ _   p  = p
mEz _ _ _ _ e  = e

-------------------------------------------------------------------------------

mapGlobs :: MapFs -> [Glob] -> [Glob]
mapGlobs fs globs = filter (not.isGDecl) globs ++
                    (map globFromDecl $ mapDecls fs globs [] [] $
                      map globToDecl $ filter isGDecl globs)

mapDecls :: MapFs -> [Glob] -> CTs -> [Decl] -> [Decl] -> [Decl]
mapDecls fs@(fS,_,_,_,_) globs cts dsigs decls =
  fS globs cts dsigs $ map (mapDecl fs globs cts dsigs') decls where
    dsigs' = filter isDSig decls ++ dsigs

mapDecl :: MapFs -> [Glob] -> CTs -> [Decl] -> Decl -> Decl
mapDecl fs@(_,fD,_,_,_) globs cts dsigs decl@(DSig _ _ _ _) = fD globs cts dsigs decl
mapDecl fs@(_,fD,_,_,_) globs cts dsigs (DAtr z pat whe)    = fD globs cts dsigs $ DAtr z pat' whe'
  where
    pat' = mapPatt  fs globs cts dsigs TAny pat
    whe' = mapWhere fs globs cts dsigs (toType dsigs pat) whe

-------------------------------------------------------------------------------

mapWhere :: MapFs -> [Glob] -> CTs -> [Decl] -> Type -> ExpWhere -> ExpWhere
mapWhere fs@(_,_,fW,_,_) globs cts dsigs xtp (ExpWhere (z,ds,e)) = fW globs cts dsigs xtp $ ExpWhere (z, ds', e')
  where
    e'  = mapExpr  fs globs cts dsigs'' xtp e
    ds' = mapDecls fs globs cts dsigs'  ds

    dsigs'' = filter isDSig ds' ++ dsigs
    dsigs'  = filter isDSig ds  ++ dsigs

-------------------------------------------------------------------------------

mapPatt :: MapFs -> [Glob] -> CTs -> [Decl] -> Type -> Patt -> Patt
mapPatt fs@(_,_,_,fP,_) globs cts dsigs xtp p = fP globs cts dsigs (rec p) where

  -- TODO: TAny
  rec (PRead  z e)     = PRead z $ mapExpr fs globs cts dsigs TAny e -- TODO: xtp
  rec (PTuple z ps)    = PTuple z $ map (mapPatt fs globs cts dsigs TAny) ps
  rec (PCall  z p1 p2) = PCall z p1' p2' where
                          p1' = mapPatt fs globs cts dsigs TAny p1
                          p2' = mapPatt fs globs cts dsigs TAny p2
  rec p                = p

-------------------------------------------------------------------------------

mapExpr :: MapFs -> [Glob] -> CTs -> [Decl] -> Type -> Expr -> Expr
mapExpr fs@(_,_,_,_,fE) globs cts dsigs xtp e = fE globs cts dsigs xtp (rec e) where
  rec (EFunc  z cs tp ups whe) = EFunc  z cs tp ups $ mapWhere fs globs ((cs,tp):cts) (arg:dsigs) xtp whe where
                                  arg = DSig z "..." cz inp
                                  (inp,xtp) = case tp of
                                                TFunc inp out -> (inp,out)
                                                otherwise     -> (TAny,TAny)
  -- TODO: TAny
  rec (EData  z hr e)          = EData  z hr $ mapExpr fs globs cts dsigs TAny e
  rec (ETuple z es)            = ETuple z $ map (mapExpr fs globs cts dsigs TAny) es
  rec (ECall  z e1 e2)         = ECall  z e1' e2' where
                                  e1' = mapExpr fs globs cts dsigs TAny e1
                                  e2' = mapExpr fs globs cts dsigs TAny e2
  rec (ECase  z e l)           = ECase  z e' l' where
    e' = mapExpr fs globs cts dsigs TAny e
    l' = map f l where
          f (pat,whe) = (pat',whe') where
            pat' = mapPatt  fs globs cts dsigs xtp pat
            whe' = mapWhere fs globs cts dsigs xtp whe
  rec e      = e

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

newtype CollectFs a = CollectFs (ExpWhere->S.Set a->S.Set a, Patt->S.Set a, Expr->S.Set a)

cEz = const S.empty
cPz = const S.empty
cWz _ x = x

cPWrite = CollectFs (cWz,cP,cEz) where
            cP :: Patt -> S.Set ID_Var
            cP (PWrite _ id) = S.singleton id
            cP _             = S.empty

collectGlobs :: Ord a => CollectFs a -> Prog -> S.Set a
collectGlobs fs globs = collectDecls fs $ map globToDecl globs

collectDecls :: Ord a => CollectFs a -> [Decl] -> S.Set a
collectDecls fs decls = S.unions $ map (collectDecl fs) decls

collectDecl :: Ord a => CollectFs a -> Decl -> S.Set a
collectDecl fs (DSig _ _ _ _)   = S.empty
collectDecl fs (DAtr _ pat whe) = S.union (collectPatt fs pat) (collectWhere fs whe)

collectWhere :: Ord a => CollectFs a -> ExpWhere -> S.Set a
collectWhere fs@(CollectFs (fW,_,_)) w@(ExpWhere (_,ds,e)) =
  fW w $ S.union (collectDecls fs ds) (collectExpr fs e)

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
