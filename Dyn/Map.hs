module Dyn.Map where

import Debug.Trace
import Data.Bool    (bool)

import Dyn.AST
import Dyn.Classes

-------------------------------------------------------------------------------

type MapFs = ( ([Glob]->Ctrs->[Decl]->Decl->[Decl]),
               ([Glob]->Ctrs->[Decl]->Type->Expr->Expr),
               ([Glob]->Ctrs->[Decl]->Patt->Patt) )
fDz _ _ _   d = [d]
fEz _ _ _ _ e = e
fPz _ _ _   p = p

-------------------------------------------------------------------------------

mapDecls :: MapFs -> [Glob] -> Ctrs -> [Decl] -> [Decl] -> [Decl]
mapDecls fs globs ctrs dsigs decls = concatMap (mapDecl fs globs ctrs dsigs') decls
  where
    dsigs' = filter isDSig decls ++ dsigs

mapDecl :: MapFs -> [Glob] -> Ctrs -> [Decl] -> Decl -> [Decl]
mapDecl fs@(fD,_,_) globs ctrs dsigs decl@(DSig _ _ _ _) = fD globs ctrs dsigs decl
mapDecl fs@(fD,_,_) globs ctrs dsigs (DAtr z pat whe)    = fD globs ctrs dsigs $ DAtr z pat' whe'
  where
    (_,pat') = mapPatt  fs globs ctrs dsigs TAny pat
    whe'     = mapWhere fs globs ctrs dsigs (toType dsigs pat) whe

-------------------------------------------------------------------------------

mapWhere :: MapFs -> [Glob] -> Ctrs -> [Decl] -> Type -> ExpWhere -> ExpWhere
mapWhere fs globs ctrs dsigs xtp (ExpWhere (z,ds,e)) = ExpWhere (z, ds', e')
  where
    e'  = mapExpr  fs globs ctrs dsigs'' xtp e
    ds' = mapDecls fs globs ctrs dsigs'  ds

    dsigs''   = filter isDSig ds' ++ dsigs
    dsigs'    = filter isDSig ds  ++ dsigs

-------------------------------------------------------------------------------

mapPatt :: MapFs -> [Glob] -> Ctrs -> [Decl] -> Type -> Patt -> ([Decl],Patt)
mapPatt fs@(_,_,fP) globs ctrs dsigs xtp p = (ds', fP globs ctrs dsigs p') where
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
mapExpr fs@(_,fE,_) globs ctrs dsigs xtp e = fE globs ctrs dsigs xtp (aux e) where
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
