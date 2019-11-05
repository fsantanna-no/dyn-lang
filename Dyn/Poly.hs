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

apply :: [Ifce] -> [Decl] -> [Decl]
apply x y = mapDecls (fD,fE TAny,fPz) x cz [] y where
  fD :: [Ifce] -> Ctrs -> [Decl] -> Decl -> [Decl]
  fD _ _ _ d@(DSig _ _ _ _)   = [d]
  fD ifces ctrs dsigs (DAtr z1 pat1 (ExpWhere (z2,ds2,e2))) = [d'] ++ dsE2' where
    d' = DAtr z1 pat1 $ ExpWhere (z2,ds2,e2')
    (dsE2',e2') = fE (pattToType dsigs pat1) ifces ctrs dsigs e2

    pattToType :: [Decl] -> Patt -> Type
    pattToType dsigs (PWrite _ id) = snd $ dsigsFind dsigs id
    --pattToType _ x = error $ pattToString True x

-------------------------------------------------------------------------

-- EVar:  pat::B = id(maximum)
-- ECall: pat::B = id2(neq) $ e2::(B,B)

fE :: Type -> [Ifce] -> Ctrs -> [Decl] -> Expr -> ([Decl],Expr)

-- pat::Bool = id(maximum)
fE xtp ifces _ dsigs (EVar z id) = (ds', EVar z id') where

  (cs,_) = dsigsFind dsigs id
  cs' = Ctrs $ Ifce.ifcesSups ifces (getCtrs cs) where

  (id',ds') =
    if null (getCtrs cs') then
      (id, [])          -- var is not poly, nothing to do
    else                -- var is poly ...
      case xtp of       --   ... and xtp is concrete -> resolve!
        TData xhr -> case ifceOrGen ifces cs id of
          Just ifc -> (posid z id, declLocals ifces dsigs z ifc xhr)
        otherwise -> (id, []) -- xtp is not concrete yet

-------------------------------------------------------------------------

-- pat1::B = id2(neq) e2::(B,B)
fE xtp ifces _ dsigs (ECall z1 (EVar z2 id2) e2) = (ds2', ECall z1 (EVar z2 id2') e2') where

  -- [("IEq",...)]
  -- a is IEq
  (cs2,_) = dsigsFind dsigs id2
  cs2' = Ctrs $ Ifce.ifcesSups ifces (getCtrs cs2) where
  cs2ids' = getCtrs cs2'

  (id2',e2',ds2') =
    if null cs2ids' || err then
      (id2,e2,[])                  -- f is not poly, nothing to do
    else                           -- f is poly ...
      case xhr of                  --   ... and xtp is concrete -> resolve!
        Just xhr -> case ifceOrGen ifces cs2' id2 of
          Nothing  -> (id2, e2T'', [])
          Just ifc -> (posid z2 id2, e2T'', declLocals ifces dsigs z2 ifc xhr)
        Nothing    -> (id2, e2A'', [])  --   ... but xtp is not concrete

  -- eq :: (a,a) -> Bool
  (_,tp2) = dsigsFind dsigs id2
  [tvar2] = toVars tp2   -- [a]

  -- eq (Bool,Boot)
  -- a is Bool
  TFunc inp2 out2 = tp2

  -- TODO: `err` is required b/c of fE that executes 2x per Expr
  -- the first execution, which fails, must keep `e2` the same
  -- (will fix when fE is called only by parents and once)
  (xhr,err) = case tpMatch (TTuple [inp2,out2])
                      ({-traceShowX (id2,toString e2,inp2,out2) $-}
                        TTuple [toType dsigs e2,xtp])
        of
          [("a", TData xhr)] -> (Just xhr, False)
          [("a", TVar  "a")] -> (Nothing,  False)

          _ -> (Nothing, True)    -- TODO: b/c of err
          --x -> error $ show x
  -- TODO: pat1 vs out2

  -- eq(dBoolIEq,...)
  e2T'' = ETuple z1 [(fromList $ map (EVar z1) dicts), e2]
          where
            -- ["dBoolIEq",...]
            dicts = map (\ifc -> "d"++concat (fromJust xhr)++ifc) cs2ids'

  -- eq(daIEq,...)
  e2A'' = ETuple z1 [(fromList $ map (EVar z1) dicts), e2]
          where
            -- ["daIEq",...]
            dicts = map (\ifc -> "d"++tvar2++ifc) cs2ids'

fE _ _ _ _ e = ([], e)

-- lt (x,y)   // ifce call to IOrd
-- f  (x,y)   // gen  call that uses IOrd inside it
ifceOrGen :: [Ifce] -> Ctrs -> ID_Var -> Maybe ID_Ifce
ifceOrGen ifces (Ctrs ifc_ids) var_id = fmap fst $ L.find (f var_id) l where
  -- [("IEq",(eq,neq)),...] $ [(eq,neq),...]         $ [IEq,...]                 $ ["IEq",...]
  l :: [(ID_Ifce,[ID_Var])]
  l = zip ifc_ids           $ map Ifce.ifceToDeclIds $ map (Ifce.ifceFind ifces) $ ifc_ids
  f field (_,fields) = elem field fields

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
declLocals :: [Ifce] -> [Decl] -> Pos -> ID_Ifce -> ID_Hier -> [Decl]
declLocals ifces dsigs z ifc_id xhr = f $ (ifc_id,dict,dcls)
                      -- ("IEq", "daIEqBool", (eq,neq))
  where
    -- "dIEqBool"
    dict = "d" ++ concat xhr ++ ifc_id
    -- [(eq,neq),...]
    dcls = Ifce.ifceToDeclIds $ Ifce.ifceFind ifces ifc_id

    -- ("IEq", "daIEqBool", (eq,neq)) -> Dict.IEq (eq,neq) = daIEqBool
    f :: (ID_Ifce, ID_Var, [ID_Var]) -> [Decl]
    f (ifc,dict,dcls) = ds ++ [d] where
      d  = DAtr z (PCall z (PCons z ["Dict",ifc]) (fromList $ map (PWrite z) $ map (posid z) dcls))
                   (ExpWhere (z, [], EVar z dict))
      ds = map g dcls where
            g id = DSig z (posid z id) cz $ mapType f $ snd $ dsigsFind dsigs id where
                    f (TVar "a") = TData xhr
                    f ttp        = ttp

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

toVars :: Type -> [ID_Var]
toVars ttp = S.toAscList $ aux ttp where
  aux TAny            = S.empty
  aux TUnit           = S.empty
  aux (TData _)       = S.empty
  aux (TVar id)       = S.singleton id
  aux (TFunc inp out) = S.union (aux inp) (aux out)
  aux (TTuple tps)    = S.unions (map aux tps)

-- (a,a) vs (Bool.True,Bool.False)  -> [(a,Bool)]
tpMatch :: Type -> Type -> [(ID_Var,Type)]
tpMatch ttp1 ttp2 = M.toAscList $ aux ttp1 ttp2 where
  aux :: Type -> Type -> M.Map ID_Var Type
  aux (TVar id)    TAny                    = M.singleton id TAny
  aux (TVar id)    (TData (hr:_))          = M.singleton id (TData [hr])
  aux (TVar id)    (TVar  id') | (id==id') = M.singleton id (TVar  id')
  --aux (TVar id)    _                       = M.singleton id ["Bool"]
  aux (TTuple ts1) (TTuple ts2)            = M.unionsWith f $ map (\(x,y)->aux x y) (zip ts1 ts2) where
                                              f TAny ttp2              = ttp2
                                              f ttp1 TAny              = ttp1
                                              f ttp1 ttp2 | ttp1==ttp2 = ttp1
  aux x y = M.empty
  --aux x y = error $ "tpMatch: " ++ show (x,y)

mapType :: (Type -> Type) -> Type -> Type
mapType f TAny             = f $ TAny
mapType f TUnit            = f $ TUnit
mapType f (TVar   id)      = f $ TVar   id
mapType f (TData  hr)      = f $ TData  hr
mapType f (TTuple ts)      = f $ TTuple (map (mapType f) ts)
mapType f (TFunc  inp out) = f $ TFunc  (mapType f inp) (mapType f out)

posid :: Pos -> ID_Var -> ID_Var
posid (l,c) id = id ++ "_" ++ show l ++ "_" ++ show c
