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
apply ifces decls = mapDecls (fD,fEz,fPz) ifces cz [] decls where
  fD :: [Ifce] -> Ctrs -> [Decl] -> Decl -> [Decl]
  fD _ _ _ d@(DSig _ _ _ _) = [d]
  fD ifces _ dsigs d@(DAtr z1 pat1 (ExpWhere (z2,ds2,e2))) = [d'] where
    dsigs' = dsigs ++ filter isDSig ds2
    d'  = DAtr z1 pat1 $ ExpWhere (z2,ds2,e2')
    e2' = poly ifces dsigs' (pattToType dsigs' pat1) e2

    pattToType :: [Decl] -> Patt -> Type
    pattToType dsigs (PWrite _ id) = snd $ dsigsFind dsigs id
    --pattToType _ x = error $ pattToString True x

-------------------------------------------------------------------------

-- EVar:  pat::B = id(maximum)
-- ECall: pat::B = id2(neq) $ e2::(B,B)

poly :: [Ifce] -> [Decl] -> Type -> Expr -> Expr

-- pat::Bool = id(maximum)
poly ifces dsigs xtp e@(EVar z id) = e' where

  (cs,_) = dsigsFind dsigs id
  cs'    = Ifce.ifcesSups ifces (getCtrs cs) where

  e' = case (cs', xtp) of
    ([], _)        -> e                          -- var is not poly, nothing to do
    (_, TData xhr) -> xxx z cs' (concat xhr) id  -- xtp is concrete
    (_, TVar _   ) -> xxx z cs' "a"          id  -- xtp is not concrete yet
    otherwise      -> e

-- pat1::B = id2(neq) e2::(B,B)
poly ifces dsigs xtp e@(ECall z1 e2@(EVar z2 id2) e3) = ECall z1 e2' e3' where

  e3' = poly ifces dsigs xtp e3

  (cs2,_) = dsigsFind dsigs id2
  cs2'    = Ifce.ifcesSups ifces (getCtrs cs2) where
  (_,tp2) = dsigsFind dsigs id2

  e2' = case (cs2', tp2) of
    ([], _)               -> e2      -- var is not poly, nothing to do
    (_,  TFunc inp2 out2) ->
      case xhr inp2 out2 of          --   ... and xtp is concrete -> resolve!
        Left ()           -> e2
        Right (Just xhr)  -> xxx z2 cs2' (concat xhr) id2
        Right Nothing     -> xxx z2 cs2' "a"          id2 -- xtp is not concrete yet
    otherwise             -> e2      -- var is not function, ignore

  xhr inp2 out2 = --traceShow (id2, toString e, toString e3, toType dsigs e3) $
    case tpMatch (TTuple [inp2             , out2])
                 (TTuple [toType dsigs e3' , xtp ]) of
      [("a", TData xhr)] -> Right $ Just xhr
      [("a", TVar  "a")] -> Right $ Nothing
      otherwise          -> Left ()
        where
          -- eq :: (a,a) -> Bool
          [tvar2] = toVars tp2   -- [a]
          -- a is Bool

poly ifces dsigs xtp (ECall z1 e2 e3) =
  ECall z1 e2' e3' where
    e2' = poly ifces dsigs TAny e2
    e3' = poly ifces dsigs TAny e3

poly ifces dsigs _ (ETuple z es) = ETuple z $ map (poly ifces dsigs TAny) es

poly ifces dsigs _ (EFunc  z1 cs1 tp1 ups1 (ExpWhere (z2,ds2,e2))) =
  EFunc z1 cs1 tp1 ups1 (ExpWhere (z2,ds2,e2')) where
    e2' = poly ifces dsigs' TAny e2 where
            dsigs' = dsigs ++ filter isDSig ds2

poly ifces dsigs xtp (ECase z e l) = ECase z e' l' where
  e' = poly ifces dsigs TAny e
  l' = map f l where
        -- TODO: pat
        f (pat, ExpWhere (z,ds,e)) = (pat, ExpWhere (z,ds,poly ifces dsigs' xtp e)) where
                                      dsigs' = dsigs ++ filter isDSig ds

poly _ _ _ e@(EArg  _)   = e
poly _ _ _ e@(EUnit _)   = e
poly _ _ _ e@(ECons _ _) = e
poly _ _ _ e@(EType _ _) = e

poly _ _ _ e = error $ show e

{-
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
                           (TTuple [toType dsigs e2,xtp])
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
            dicts = map (\ifc -> "d"++ifc++concat (fromJust xhr)) cs2ids'

  -- eq(daIEq,...)
  e2A'' = ETuple z1 [(fromList $ map (EVar z1) dicts), e2]
          where
            -- ["daIEq",...]
            dicts = map (\ifc -> "d"++ifc++tvar2) cs2ids'
-}

{-
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
    dict = "d" ++ ifc_id ++ concat xhr
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

mapType :: (Type -> Type) -> Type -> Type
mapType f TAny             = f $ TAny
mapType f TUnit            = f $ TUnit
mapType f (TVar   id)      = f $ TVar   id
mapType f (TData  hr)      = f $ TData  hr
mapType f (TTuple ts)      = f $ TTuple (map (mapType f) ts)
mapType f (TFunc  inp out) = f $ TFunc  (mapType f inp) (mapType f out)

posid :: Pos -> ID_Var -> ID_Var
posid (l,c) id = id ++ "_" ++ show l ++ "_" ++ show c
-}


-------------------------------------------------------------------------


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
  aux (TTuple ts1) TAny                    = aux (TTuple ts1) (TTuple $ replicate (length ts1) TAny)
  aux x y = M.empty
  --aux x y = error $ "tpMatch: " ++ show (x,y)

toVars :: Type -> [ID_Var]
toVars ttp = S.toAscList $ aux ttp where
  aux TAny            = S.empty
  aux TUnit           = S.empty
  aux (TData _)       = S.empty
  aux (TVar id)       = S.singleton id
  aux (TFunc inp out) = S.union (aux inp) (aux out)
  aux (TTuple tps)    = S.unions (map aux tps)

xxx z ifcs suf id = ECall z (EVar z $ id++"'")
                            (fromList $ map (EVar z) $ map toID $ ifcs) where
                              toID id = "d" ++ id ++ suf
