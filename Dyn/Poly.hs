module Dyn.Poly where

import Debug.Trace
import Data.Maybe               (fromJust)
import qualified Data.List as L (find)
import qualified Data.Set  as S
import qualified Data.Map  as M

import Dyn.AST
import Dyn.Classes
import qualified Dyn.Ifce as Ifce

-------------------------------------------------------------------------------

-- TODO: polyPatt

-- [Ifce]: known ifces
-- [Decl]: known DSig decls
-- Type:   expected type
-- [Decl]: decls to transform
-- [Decl]: transformed decls (maybe the same)
--
polyDecls :: [Ifce] -> [Decl] -> [Decl] -> [Decl]
polyDecls ifces dsigs decls = map (polyDecl ifces dsigs') decls where
  dsigs' = dsigs ++ filter isDSig decls

-------------------------------------------------------------------------------

polyDecl :: [Ifce] -> [Decl] -> Decl -> Decl

polyDecl _  _ d@(DSig _ _ _) = d

polyDecl ifces dsigs (DAtr z pat whe) =
  DAtr z pat $ polyExpWhere ifces dsigs (pattToType dsigs pat) whe
  -- TODO: pat

-------------------------------------------------------------------------

polyExpWhere :: [Ifce] -> [Decl] -> Type -> ExpWhere -> ExpWhere

polyExpWhere ifces dsigs tp (ExpWhere (z,e,ds)) =
  ExpWhere (z,e',eds'++ds') where
    (e',eds') = polyExpr  ifces (filter isDSig ds++dsigs) tp e
    ds'       = polyDecls ifces dsigs ds

-------------------------------------------------------------------------

-- EVar:  pat::B = id(maximum)
-- ECall: pat::B = id2(neq) $ e2::(B,B)

polyExpr :: [Ifce] -> [Decl] -> Type -> Expr -> (Expr,[Decl])

-- pat::Bool = id(maximum)
polyExpr ifces dsigs xtp (EVar z id) = (EVar z id', ds') where

  (id',ds') =
    if null cs then
      (id, [])          -- var is not poly, nothing to do
    else                -- var is poly ...
      case xtp of       --   ... and xtp is concrete -> resolve!
        Type (_,TData xhr,_) -> (posid z id, declLocals ifces dsigs z ifc_ids xhr)
        otherwise            -> (id, []) -- xtp is not concrete yet

  -- maximum :: a where a is IBounded
  Type (_,ttp,cs) = dsigFind dsigs id

  [tvar] = toVars ttp   -- [a]

  -- [("IBounded",...)]
  ifc_ids = snd $ fromJust $ L.find ((==tvar).fst) cs

-------------------------------------------------------------------------

-- pat1::B = id2(neq) e2::(B,B)
polyExpr ifces dsigs _ (ECall z1 (EVar z2 id2) e2) = (ECall z1 (EVar z2 id2') e2''', e2ds' ++ ds2') where

  (e2', e2ds') = polyExpr ifces dsigs tz e2

  (id2',e2''',ds2') =
    if null cs2 then
      (id2,e2',[])                  -- f is not poly, nothing to do
    else                            -- f is poly ...
      case xhr of                   --   ... and xtp is concrete -> resolve!
        Just xhr -> (posid z2 id2, e2T'', declLocals ifces dsigs z2 ifc_ids xhr)
        Nothing  -> (id2,e2A'',[])  --   ... but xtp is not concrete

  -- eq :: (a,a) -> Bool
  Type (_,ttp2,cs2) = dsigFind dsigs id2

  [tvar2] = toVars ttp2   -- [a]

  -- [("IEq",...)]
  -- a is IEq
  ifc_ids = snd $ fromJust $ L.find ((==tvar2).fst) cs2

  -- eq (Bool,Boot)
  -- a is Bool
  TFunc inp2 out2 = ttp2
  xhr = case ttpMatch inp2 ({-traceShowX id2 $-} toTType dsigs e2) of
          [("a", TVar  "a")] -> Nothing
          [("a", TData xhr)] -> Just xhr
  -- TODO: pat1 vs out2

  -- eq(dBoolIEq,...)
  e2T'' = ETuple z1 [(fromList $ map (\d-> ECall z1 (EVar z1 d) (EUnit z1)) dicts), e2']
          where
            -- ["dBoolIEq",...]
            dicts = map (\ifc -> "d"++concat (fromJust xhr)++ifc) ifc_ids

  -- eq(daIEq,...)
  e2A'' = ETuple z1 [(fromList $ map (EVar z1) dicts), e2']
          where
            -- ["daIEq",...]
            dicts = map (\ifc -> "d"++tvar2++ifc) ifc_ids

polyExpr ifces dsigs _ (ECall z e1 e2) =
  (ECall z e1' e2', e1ds'++e2ds') where
    (e1',e1ds') = polyExpr ifces dsigs tz e1
    (e2',e2ds') = polyExpr ifces dsigs tz e2

-------------------------------------------------------------------------

polyExpr ifces dsigs _ (ETuple z es) =
  (ETuple z es', concat ds') where
    (es',ds') = unzip $ map (polyExpr ifces dsigs tz) es

polyExpr ifces dsigs _ (EFunc z tp ups whe) =
  (EFunc z tp ups whe', []) where
    whe' = polyExpWhere ifces dsigs tz whe

polyExpr ifces dsigs _ (ECase z e cses) =
  (ECase z e' (zip pats whes'), eds') where
    (e',eds') = polyExpr     ifces dsigs tz e
    whes'     = map (polyExpWhere ifces dsigs tz) whes
    (pats,whes) = unzip cses -- TODO pats

polyExpr _ _ _ e@(ECons  _ _) = (e, [])
polyExpr _ _ _ e@(EUnit  _)   = (e, [])
polyExpr _ _ _ e@(EArg   _)   = (e, [])
polyExpr _ _ _ e@(EError _ _) = (e, [])
polyExpr _ _ _ e = error $ toString e
--polyExpr _ _ _ e = (e, [])

-------------------------------------------------------------------------
-------------------------------------------------------------------------
-- [
--  "min :: Bool",
--  "max :: Bool",
--  "Dict.IBounded (min,max) = dIBoundedBool",
--  "eq :: ((Bool,Bool)->Bool)",
--  "neq ::((Bool,Bool)->Bool)",
--  "Dict.IEq (eq,neq) = dIEqBool",
--  ...
-- ]
declLocals :: [Ifce] -> [Decl] -> Pos -> [ID_Ifce] -> ID_Hier -> [Decl]
declLocals ifces dsigs z ifc_ids xhr = concatMap f $
                      -- [("IEq", "daIEqBool", (eq,neq)),...]
                      zip3 ifc_ids dicts dclss
  where
    -- ["dIEqBool",...]
    dicts = map (\ifc -> "d"++concat xhr++ifc) ifc_ids
    -- [(eq,neq),...]
    dclss = map Ifce.ifceToDeclIds $ map (Ifce.ifceFind ifces) ifc_ids

    -- ("IEq", "daIEqBool", (eq,neq)) -> Dict.IEq (eq,neq) = daIEqBool
    f :: (ID_Ifce, ID_Var, [ID_Var]) -> [Decl]
    f (ifc,dict,dcls) = ds ++ [d] where
      d  = DAtr z (PCall z (PCons z ["Dict",ifc]) (fromList $ map (PWrite z) $ map (posid z) dcls))
                   (ExpWhere (z, ECall z (EVar z dict) (EUnit z), []))
      ds = map g dcls where
            g id = DSig z (posid z id) $ mapType f $ dsigFind dsigs id where
                    f (TVar "a") = TData xhr
                    f ttp        = ttp

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

dsigFind :: [Decl] -> ID_Var -> Type
dsigFind dsigs id = case L.find f dsigs of
                      Nothing            -> Type (pz,TAny,cz)
                      Just (DSig _ _ tp) -> tp
                    where
                      f :: Decl -> Bool
                      f (DSig _ x _) = (id == x)

toTType :: [Decl] -> Expr -> TType
toTType _  (EArg   _)     = TAny
toTType ds (EVar   _ id)  = ttp where Type (_,ttp,_) = dsigFind ds id
toTType _  (ECons  _ hr)  = TData hr
toTType ds (ETuple _ es)  = TTuple $ map (toTType ds) es
toTType ds (ECall  _ f _) = case toTType ds f of
                              TAny        -> TAny
                              TFunc _ out -> out
                              TData hr    -> TData hr
toTType _  e = error $ "toTType: " ++ toString e

toVars :: TType -> [ID_Var]
toVars ttp = S.toAscList $ aux ttp where
  aux TAny            = S.empty
  aux TUnit           = S.empty
  aux (TData _)       = S.empty
  aux (TVar id)       = S.singleton id
  aux (TFunc inp out) = S.union (aux inp) (aux out)
  aux (TTuple tps)    = S.unions (map aux tps)

-- (a,a) vs (Bool.True,Bool.False)  -> [(a,Bool)]
ttpMatch :: TType -> TType -> [(ID_Var,TType)]
ttpMatch ttp1 ttp2 = M.toAscList $ aux ttp1 ttp2 where
  aux :: TType -> TType -> M.Map ID_Var TType
  aux (TVar id)    (TData (hr:_))          = M.singleton id (TData [hr])
  aux (TVar id)    (TVar  id') | (id==id') = M.singleton id (TVar  id')
  --aux (TVar id)    _                     = M.singleton id ["Bool"]
  aux (TTuple ts1) (TTuple ts2)            = M.unionsWith f $ map (\(x,y)->aux x y) (zip ts1 ts2)
                                      where f hr1 hr2 | hr1==hr2 = hr1
  --aux x y = M.singleton "a" (TData ["Bool"])
  aux x y = error $ "ttpMatch: " ++ show (x,y)

pattToType :: [Decl] -> Patt -> Type
pattToType dsigs (PWrite _ id) = dsigFind dsigs id
--pattToType _ x = error $ pattToString True x

mapType :: (TType -> TType) -> Type -> Type
mapType f (Type (z,ttp,cs)) = Type (z, aux f ttp, cs) where
  aux f TAny             = f $ TAny
  aux f TUnit            = f $ TUnit
  aux f (TVar   id)      = f $ TVar   id
  aux f (TData  hr)      = f $ TData  hr
  aux f (TTuple ts)      = f $ TTuple (map (aux f) ts)
  aux f (TFunc  inp out) = f $ TFunc  (aux f inp) (aux f out)

posid :: Pos -> ID_Var -> ID_Var
posid (l,c) id = id ++ "_" ++ show l ++ "_" ++ show c
