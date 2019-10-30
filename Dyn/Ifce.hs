module Dyn.Ifce where

import Debug.Trace
import Data.Bool                (bool)
import Data.Maybe               (fromJust)
import qualified Data.List as L (find, sort)
import qualified Data.Set  as S
import qualified Data.Map  as M
import Text.RawString.QQ

import Dyn.AST
import Dyn.Classes

-------------------------------------------------------------------------------

ifceFind :: [Ifce] -> ID_Ifce -> Ifce
ifceFind ifces ifc = fromJust $ L.find f ifces where
                      f :: Ifce -> Bool
                      f (Ifce (_,id,_,_)) = (id == ifc)

-- IEq -> [eq,neq]
ifceToDeclIds :: Ifce -> [ID_Var]
ifceToDeclIds (Ifce (_,_,_,dcls)) = map getId $ filter isDSig dcls where
                                      getId (DSig _ id _) = id

-- [...] -> ["IEq"] -> ["IEq","IOrd"] -- (sorted)
ifcesSups :: [Ifce] -> [ID_Ifce] -> [ID_Ifce]
ifcesSups ifces []  = []
ifcesSups ifces ids = L.sort $ ifcesSups ifces ids' ++ ids where
                        ids' = concatMap (f . (ifceFind ifces)) ids
                        f (Ifce (_,ifc,[],     _)) = []
                        f (Ifce (_,ifc,[(_,l)],_)) = l
                        f _ = error $ "TODO: multiple constraints"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- interface IEq for a
--  dIEq = Dict.IEq (eq,neq)              -- : declare instance dict if all defaults are implemented
--  <...>                                 -- : modify nested impls which become globals

ifceToDecls :: [Ifce] -> Ifce -> [Decl]
ifceToDecls ifces me@(Ifce (z,ifc_id,_,decls)) = dict ++ decls' where

  -- dIEq = Dict.IEq (eq,neq)
  -- (only if all methods are implemented)
  dict :: [Decl]
  dict = bool [] [datr] has_all_impls where
          datr = DAtr z (PWrite z ("d"++ifc_id)) (ExpWhere (z,f,[])) where
            f = EFunc z tz [] (ExpWhere (z,d,[]))
            d = ECall z (ECons z ["Dict",ifc_id])
                        (fromList $ map (EVar z) $ ifceToDeclIds me)

          has_all_impls = (length dsigs == length datrs) where
                            (dsigs, datrs) = declsSplit decls

  decls' = map (expandDecl ifces (ifc_id,[])) decls

-------------------------------------------------------------------------------

-- implemenation of IEq for Bool
-- implemenation of IEq for a where a is IXxx
--  dIEqBool = Dict.IEq (eq,neq) where    -- : declare instance dict with methods
--              <...>                     -- :   with nested impls to follow only visible here

implToDecls :: [Ifce] -> Impl -> [Decl]
implToDecls ifces (Impl (z,ifc,tp@(Type (_,_,cs)),decls)) = [dict] where

  -- dIEqBool = func -> Dict.IEq (eq,neq) where eq=<...> daIXxx=...;
  -- func b/c of HKT that needs a closure with parametric dictionary
  dict = DAtr z (PWrite z ("d"++ifc++toString' tp)) (ExpWhere (z,f,[])) where
          f = EFunc z tz [] (ExpWhere (z,d,decls'++[ups']))
          d = ECall z (ECons z ["Dict",ifc])
                      (fromList $ map (EVar z) $ ifceToDeclIds ifce)

  -- {daIXxx} // implementation of IOrd for a where a is IXxx
  ups' = DAtr z (fromList $ map (PWrite z) $ L.sort $ map ("da"++) imp_ids)
                (ExpWhere (z,EArg z,[]))

  toString' (Type (_, TData hr, _      )) = concat hr
  toString' (Type (_, TVar _,   [(_,l)])) = concat l

  -- eq = <...>
  decls' = map (expandDecl ifces (id,imp_ids)) decls where
            Ifce (_,id,_,_) = ifce    -- id:  from interface

  imp_ids = case cs of          -- ids: from instance constraints
              []      -> []
              [(_,l)] -> l
              _       -> error "TODO: multiple vars"

  ifce = fromJust $ L.find h ifces where
          h :: Ifce -> Bool
          h (Ifce (_,id,_,_)) = (id == ifc)

-------------------------------------------------------------------------------

-- [Ifce]:              known interfaces
-- (ID_ifce,[ID_Ifce]): iface constraint // impl extra constraints
-- Decl:                decl to expand
-- Decl:                expanded decl
expandDecl :: [Ifce] -> (ID_Ifce,[ID_Ifce]) -> Decl -> Decl

expandDecl ifces (ifc_id,imp_ids) (DSig z1 id1 (Type (z2,ttp2,cs2))) =
  DSig z1 id1 (Type (z2,ttp2,cs2')) where
    -- TODO: a?
    cs2' = ("a", ifcesSups ifces (ifc_id:imp_ids)) : cs2

-- IBounded: minimum/maximum
expandDecl _ _ decl@(DAtr _ _ (ExpWhere (_,econst,_))) | isConst econst = decl where
  isConst (EUnit  _)      = True
  isConst (ECons  _ _)    = True
  isConst (ETuple _ es)   = all isConst es
  isConst (ECall  _ f ps) = isConst f && isConst ps
  isConst _               = False

--  eq = func :: ((a,a) -> Bool) ->       -- : insert a is IEq/IXxx
--    ret where
--      <...>
--      ... = (p1,...pN)                  -- : restore original args
--      (fN,...,gN) = dN                  -- : restore iface functions from dicts
--      ((d1,...,dN), (p1,...,pN)) = ...  -- : split dicts / args from instance call
expandDecl ifces
           (ifc_id,imp_ids)
           (DAtr z1 e1
            (ExpWhere (z2,
              EFunc z3 (Type (z4,TFunc inp4 out4,cs4)) [] (ExpWhere (z5,e5,ds5)),
              ds2))) =
  DAtr z1 e1
    (ExpWhere (z2,
               EFunc z3 (Type (z4,TFunc inp4 out4,cs4NImps')) ups3' (ExpWhere (z5,e5,ds5')),
               ds2))
  where
    --  a where a is (IEq,IOrd)
    -- TODO: a?
    -- TODO: ctrsUnion
    cs4NImps' = ("a", ifcesSups ifces [ifc_id])         : cs4
    cs4YImps  = ("a", ifcesSups ifces (ifc_id:imp_ids)) : cs4

    -- {daIXxx} // implementation of IOrd for a where a is IXxx
    ups3' = map (\id -> (id,EUnit pz)) $ L.sort $ map ("da"++) imp_ids

    --  <...>               -- original
    --  (f1,...,g1) = d1
    --  (fN,...,gN) = dN
    --  ... = args          -- AUTO
    --  ((d1,...,dN), args) = ...
    ds5' = ds5 ++ fsDicts5 ++ [
      DAtr z1 (PArg z1)                             (ExpWhere (z1,EVar z1 "args",[])),
      DAtr z1 (PTuple z1 [dicts5,PWrite z1 "args"]) (ExpWhere (z1,EArg z1,[]))
     ]

    -- [Dict.IEq (eq,neq) = daIEq]
    fsDicts5 :: [Decl]
    fsDicts5 = map f (dicts cs4YImps) where
      f :: (ID_Var,ID_Ifce,[ID_Var]) -> Decl
      f (var,ifc,ids) = DAtr z1 pat (ExpWhere (z1,exp,[])) where
        -- Dict.IEq (eq,neq)
        pat :: Patt
        pat = PCall z1 (PCons z1 ["Dict",ifc]) (fromList $ map (PWrite z1) ids)
        -- daIEq
        exp :: Expr
        exp = EVar z1 $ 'd':var++ifc

    -- (d1,...,dN)
    -- csNImps: exclude implementation constraints since dicts come from closure
    dicts5 :: Patt
    dicts5 = fromList $ map (PWrite z1) $ L.sort $ map (\(var,ifc,_) -> 'd':var++ifc) (dicts cs4NImps')
                                                  -- [daIEq,daIShow,dbIEq,...]

    -- [ (a,IEq,[eq,neq]), (a,IOrd,[...]), (b,...,...), ...]
    dicts :: TCtrs -> [(ID_Var,ID_Ifce,[ID_Var])]
    dicts cs = concatMap f cs where
      -- (a,[IEq,IShow]) -> [(a,IEq,[eq,neq]), (a,IOrd,[lt,gt,lte,gte]]
      f :: (ID_Var,[ID_Ifce]) -> [(ID_Var,ID_Ifce,[ID_Var])]
      f (var,ifcs) = map h $ map g ifcs where
                      h :: (ID_Ifce,[ID_Var]) -> (ID_Var,ID_Ifce,[ID_Var])
                      h (ifc,ids) = (var,ifc,ids)

      -- IEq -> (IEq, [eq,neq])
      g :: ID_Ifce -> (ID_Ifce,[ID_Var])
      g ifc = (ifc, ifceToDeclIds $ ifceFind ifces ifc)

expandDecl _ _ decl = error $ toString decl

-------------------------------------------------------------------------------
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

  (id',ds') = if null cs then
                (id, [])
              else
                (posid z id, declLocals ifces dsigs z ifc_ids xhr)

  -- x :: Bool = maximum
  Type (_,TData xhr,_) = xtp

  -- maximum :: a where a is IBounded
  Type (_,ttp,cs) = dsigFind dsigs id

  [tvar] = toVars ttp   -- [a]

  -- [("IBounded",...)]
  ifc_ids = snd $ fromJust $ L.find ((==tvar).fst) cs

-------------------------------------------------------------------------

-- pat1::B = id2(neq) e2::(B,B)
polyExpr ifces dsigs _ (ECall z1 (EVar z2 id2) e2) = (ECall z1 (EVar z2 id2') e2''', e2ds' ++ ds2') where

  (e2', e2ds') = polyExpr ifces dsigs tz e2

  (id2',e2''',ds2') = if null cs2 then
                        (id2,e2',[])
                      else
                        (posid z2 id2, e2'', declLocals ifces dsigs z2 ifc_ids xhr)

  -- eq :: (a,a) -> Bool
  Type (_,ttp2,cs2) = dsigFind dsigs id2

  [tvar2] = toVars ttp2   -- [a]

  -- [("IEq",...)]
  -- a is IEq
  ifc_ids = snd $ fromJust $ L.find ((==tvar2).fst) cs2

  -- ["dIEqBool",...]
  dicts = map (\ifc -> "d"++ifc++concat xhr) ifc_ids

  -- eq (Bool,Boot)
  -- a is Bool
  TFunc inp2 out2 = ttp2
  [("a", TData xhr)] = traceShow (id2,inp2,e2) ttpMatch inp2 (traceShowX e2 $ toTType dsigs e2)
  -- TODO: pat1 vs out2

  -- eq(dIEqBool,...)
  e2'' = ETuple z1 [(fromList $ map (\d-> ECall z1 (EVar z1 d) (EUnit z1)) dicts), e2']

-------------------------------------------------------------------------

polyExpr ifces dsigs _ (ETuple z es) =
  (ETuple z es', concat ds') where
    (es',ds') = unzip $ map (polyExpr ifces dsigs tz) es

polyExpr ifces dsigs _ (EFunc  z tp ups whe) =
  (EFunc z tp ups whe', []) where
    whe' = polyExpWhere ifces dsigs tz whe

polyExpr _ _ _ e = (e, [])

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
    dicts = map (\ifc -> "d"++ifc++concat xhr) ifc_ids
    -- [(eq,neq),...]
    dclss = map ifceToDeclIds $ map (ifceFind ifces) ifc_ids

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
  aux (TVar id)    (TData (hr:_)) = M.singleton id (TData [hr])
  --aux (TVar id)    (TVar  id)     = M.singleton id ["Bool"]
  --aux (TVar id)    _              = M.singleton id ["Bool"]
  aux (TTuple ts1) (TTuple ts2)   = M.unionsWith f $ map (\(x,y)->aux x y) (zip ts1 ts2)
                                      where f hr1 hr2 | hr1==hr2 = hr1
  aux x y = error $ "ttpMatch: " ++ show (x,y)

pattToType :: [Decl] -> Patt -> Type
pattToType dsigs (PWrite _ id) = dsigFind dsigs id
--pattToType _ x = error $ pattToString True x

mapType :: (TType -> TType) -> Type -> Type
mapType f (Type (z,ttp,cs)) = Type (z, aux f ttp, cs) where
  aux f TAny             = f $ TAny
  aux f (TVar   id)      = f $ TVar   id
  aux f (TData  hr)      = f $ TData  hr
  aux f (TTuple ts)      = f $ TTuple (map (aux f) ts)
  aux f (TFunc  inp out) = f $ TFunc  (aux f inp) (aux f out)
  aux _ x = error $ show x

posid :: Pos -> ID_Var -> ID_Var
posid (l,c) id = id ++ "_" ++ show l ++ "_" ++ show c
