module Dyn.Poly where

import Debug.Trace
import qualified Data.Set  as S
import qualified Data.Map  as M

import Dyn.AST
import Dyn.Classes
import Dyn.Map
import qualified Dyn.Ifce as Ifce

-------------------------------------------------------------------------------

apply :: [Glob] -> [Glob] -> [Glob]
apply origs globs = mapGlobs (mSz,mDz,mWz,mPz,mE) origs globs where

-------------------------------------------------------------------------

-- EVar:  pat::B = id(maximum)
-- ECall: pat::B = id2(neq) $ e2::(B,B)

mE :: [Glob] -> CTs -> [Decl] -> Type -> Expr -> Expr

-- pat::Bool = id(maximum)
mE globs cts dsigs xtp e@(EVar z id) = e' where

  (dcs,dtp) = dsigsFind dsigs id
  dcs'      = Ifce.ifcesSups globs (getCtrs dcs) where

  e' = case (xtp, dcs',dtp) of

    -- local poly var
    --    v where v::a
    (_, [], TVar "a")     -> e

    -- id is not poly, nothing to do, just keep it
    --    Bool.True
    (_, [], _)            -> e

    -- xtp is concrete, instantiate e with it
    --    maximum :: Bool
    (TData xhr2 _, _, _)  -> f (concat xhr2)     -- TODO: TData ofs

    -- xtp is not concrete yet
    -- .
    --(_, TVar "a", [])    -> error $ toString e --f "a"

    otherwise             -> e

    where
      f suf = ECall z (EVar z $ id++"'")
                      (fromList $ map g $ dcs') where
        g ifc = if elem suf (getRecDatas globs dtp) then
                  ECall z (EVar z "snd")
                          (ECall z (EVar z $ "d"++ifc++suf) (EVar z id))
                else
                  EVar z $ "d" ++ ifc ++ suf

-- pat1::B = id2(neq) e2::(B,B)
mE globs _ dsigs xtp e@(ECall z1 e2@(EVar z2 id2) e3) = ECall z1 e2' e3 where

  TFunc inp2 out2 = tp2
  (cs2,tp2)       = dsigsFind dsigs id2
  cs2'            = Ifce.ifcesSups globs (getCtrs cs2) where

  e2' = case (cs2', tp2) of

    -- call id2 is not poly, nothing to do, just keep it
    --    isTrue Bool.True
    ([], _)                 -> e2

    -- call id2 is poly
    otherwise -> case tpMatch (TTuple [inp2            , out2]) -- TODO: variance
                              (TTuple [toType dsigs e3 , xtp ])
                 of

      -- ... and xtp is concrete -> resolve!
      --    eq (Bool.True,Bool.False)
      --    (eq' dIEqBool) (Bool.True,Bool.False)
      [("a", TUnit)]        -> toSimpleF' $ concat ["Unit"]
      [("a", TData xhr _)]  -> toSimpleF' $ concat xhr   -- TODO: _

      -- ... but xtp is not concrete yet
      [("a", TVar  "a")]

        -- function id2 receives recursive generic
        --    f :: (List of a -> ...) where a is IEnum
        --    (f' hash) l
        | elem "a" recs2    -> toComplexF' "a"

        -- function id2 receives non-recursive generic
        --    eq (x,y) where x::a, y::a
        --    (eq' dIEqa) (x,y)
      [("a", TVar  "a")]    -> toSimpleF'  "a"

  recs2 = getRecDatas globs tp2

  toSimpleF' suf = ECall z2 (EVar z2 $ id2++"'")
                      (fromList $ map (EVar z2) $ map toID $ cs2') where
                      toID ifc = "d" ++ ifc ++ suf -- dIEqa / dIEqBool

  toComplexF' suf = ECall z2 (EVar z2 $ id2++"'")
                      (fromList $ map f $ cs2') where
    f ifc = EFunc z2 cz TAny [] -- f' (func -> (k,v) -> (hash (ds_IEnum k), v)
              (ExpWhere (z2, [],
                ECase z2 (EArg z2) [
                  (PTuple z2 [PWrite z2 "k",PWrite z2 "v"],
                    ExpWhere (z2, [],
                      ETuple z2 [ECall z2 (EVar z2 "getHash")
                                  (ETuple z2 [EVar z2 ("ds_"++ifc),EVar z2 "k"]),
                                 EVar z2 "v"]))
                ]))

mE _ _ _ _ e = e

-------------------------------------------------------------------------

-- (a,a) vs (Bool.True,Bool.False)  -> [(a,Bool)]
tpMatch :: Type -> Type -> [(ID_Var,Type)]
tpMatch tp1 tp2 = {-traceShow ("MATCH",toString tp1,toString tp2) $-} M.toAscList $ aux tp1 tp2 where
  aux :: Type -> Type -> M.Map ID_Var Type
  aux (TVar id)      TAny                    = M.singleton id TAny
  aux (TVar id)      TUnit                   = M.singleton id TUnit
  aux (TVar id)      (TData (hr:_) ofs)      = M.singleton id (TData [hr] ofs)    -- TODO: ofs
  aux (TVar id)      (TVar  id') | (id==id') = M.singleton id (TVar  id')
  --aux (TVar id)    _                       = M.singleton id ["Bool"]
  aux (TData _ ofs1) (TData _ ofs2)          = unions $ zip ofs1 ofs2
  aux (TTuple ts1)   (TTuple ts2)            = unions $ zip ts1 ts2
  aux (TTuple ts1)   TAny                    = aux (TTuple ts1) (TTuple $ replicate (length ts1) TAny)
  aux x y = M.empty
  --aux x y = error $ "tpMatch: " ++ show (x,y)

  unions ls = M.unionsWith f $ map (\(x,y)->aux x y) ls where
                f TAny     tp2       = tp2
                f tp1      TAny      = tp1
                f tp1      (TVar _)  = tp1
                f (TVar _) tp2       = tp2
                f tp1 tp2 | tp1==tp2 = tp1
                f tp1 tp2 = error $ show (toString tp1, toString tp2)

getRecDatas :: [Glob] -> Type -> [ID_Var]
getRecDatas globs (TData (hr:_) [(TVar "a")]) = case dataFind globs hr of
                                                  Data (_,True,_,_,_) -> ["a"]
                                                  otherwise           -> []
getRecDatas globs (TTuple ts)                 = concatMap (getRecDatas globs) ts
getRecDatas globs (TFunc inp out)             = getRecDatas globs inp ++ getRecDatas globs out
getRecDatas _ _                               = []
