module Dyn.Map where

import Debug.Trace
import Data.Bool    (bool)
import qualified Data.Set  as S

import Dyn.AST
import Dyn.Classes

-------------------------------------------------------------------------------

type MapFs = ( ([Glob]->Ctrs->[Decl]->[Decl]->[Decl]),
               ([Glob]->Ctrs->[Decl]->Decl->[Decl]),
               ([Glob]->Ctrs->[Decl]->Type->ExpWhere->ExpWhere),
               ([Glob]->Ctrs->[Decl]->Patt->Patt),
               ([Glob]->Ctrs->[Decl]->Type->Expr->Expr))
mSz _ _ _   ds = ds
mDz _ _ _   d  = [d]
mWz _ _ _ _ w  = w
mPz _ _ _   p  = p
mEz _ _ _ _ e  = e

-------------------------------------------------------------------------------

mapGlobs :: MapFs -> [Glob] -> [Glob] -> [Glob]
mapGlobs fs origs globs = map globFromDecl $ mapDecls fs origs cz [] (map globToDecl globs)

mapDecls :: MapFs -> [Glob] -> Ctrs -> [Decl] -> [Decl] -> [Decl]
mapDecls fs@(fS,_,_,_,_) globs ctrs dsigs decls =
  fS globs ctrs dsigs $ concatMap (mapDecl fs globs ctrs dsigs') decls where
    dsigs' = filter isDSig decls ++ dsigs

mapDecl :: MapFs -> [Glob] -> Ctrs -> [Decl] -> Decl -> [Decl]
mapDecl fs@(_,fD,_,_,_) globs ctrs dsigs decl@(DSig _ _ _ _) = fD globs ctrs dsigs decl
mapDecl fs@(_,fD,_,_,_) globs ctrs dsigs (DAtr z pat whe)    = fD globs ctrs dsigs $ DAtr z pat' whe'
  where
    (_,pat') = mapPatt  fs globs ctrs dsigs TAny pat
    whe'     = mapWhere fs globs ctrs dsigs (toType dsigs pat) whe

-------------------------------------------------------------------------------

mapWhere :: MapFs -> [Glob] -> Ctrs -> [Decl] -> Type -> ExpWhere -> ExpWhere
mapWhere fs@(_,_,fW,_,_) globs ctrs dsigs xtp (ExpWhere (z,ds,e)) = fW globs ctrs dsigs xtp $ ExpWhere (z, ds', e')
  where
    e'  = mapExpr  fs globs ctrs dsigs'' xtp e
    ds' = mapDecls fs globs ctrs dsigs'  ds

    dsigs''   = filter isDSig ds' ++ dsigs
    dsigs'    = filter isDSig ds  ++ dsigs

-------------------------------------------------------------------------------

mapPatt :: MapFs -> [Glob] -> Ctrs -> [Decl] -> Type -> Patt -> ([Decl],Patt)
mapPatt fs@(_,_,_,fP,_) globs ctrs dsigs xtp p = (ds', fP globs ctrs dsigs p') where
  (ds',p') = aux p

  -- TODO: TAny
  aux (PWrite z id)    = (dsig, PWrite z id) where
                          dsig = bool [DSig z id cz xtp] [] (xtp==TAny)
  aux (PRead  z e)     = ([], PRead z $ mapExpr fs globs ctrs dsigs TAny e) -- TODO: xtp
  aux (PTuple z ps)    = (concat ds', PTuple z ps') where
                          (ds',ps') = unzip $ map (mapPatt fs globs ctrs dsigs TAny) ps
  aux (PCall  z p1 p2) = (ds2'++ds1', PCall z p1' p2') where
                          (ds1',p1') = mapPatt fs globs ctrs dsigs TAny p1
                          (ds2',p2') = mapPatt fs globs ctrs dsigs TAny p2
  aux p                = ([], p)

-------------------------------------------------------------------------------

mapExpr :: MapFs -> [Glob] -> Ctrs -> [Decl] -> Type -> Expr -> Expr
mapExpr fs@(_,_,_,_,fE) globs ctrs dsigs xtp e = fE globs ctrs dsigs xtp (aux e) where
  aux (EFunc  z cs tp ups whe) = EFunc  z cs tp ups $ mapWhere fs globs ctrs' (arg:dsigs) xtp whe where
                                  ctrs' = Ctrs $ getCtrs ctrs ++ getCtrs cs
                                  arg = DSig z "..." cz inp
                                  (inp,xtp) = case tp of
                                                TFunc inp out -> (inp,out)
                                                otherwise     -> (TAny,TAny)
  -- TODO: TAny
  aux (EData  z hr e)          = EData  z hr $ mapExpr fs globs ctrs dsigs TAny e
  aux (ETuple z es)            = ETuple z $ map (mapExpr fs globs ctrs dsigs TAny) es
  aux (ECall  z e1 e2)         = ECall  z e1' e2' where
                                  e1' = mapExpr fs globs ctrs dsigs TAny e1
                                  e2' = mapExpr fs globs ctrs dsigs TAny e2
  aux (ECase  z e l)           = ECase  z e' l' where
    e'         = mapExpr fs globs ctrs dsigs TAny e
    (ps, ws)   = unzip l
    (dss',ps') = unzip $ map (mapPatt fs globs ctrs dsigs xtp) ps
    ws'        = map f (zip dss' ws) where
                  f (ds,w) = mapWhere fs globs ctrs dsigs' xtp w where
                              dsigs' = (filter isDSig ds) ++ dsigs
    l'         = zip ps' ws'
  aux e                        = e

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

newtype CollectFs a = CollectFs (ExpWhere->S.Set a->S.Set a, Patt->S.Set a, Expr->S.Set a)

cEz = const S.empty
cPz = const S.empty
cWz _ x = x

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
