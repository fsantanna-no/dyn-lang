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

mE :: [Glob] -> Ctrs -> [Decl] -> Type -> Expr -> Expr

-- pat::Bool = id(maximum)
mE globs _ dsigs xtp e@(EVar z id) = e' where

  (cs,_) = dsigsFind dsigs id
  cs'    = Ifce.ifcesSups globs (getCtrs cs) where

  e' = case (cs', xtp) of
    ([], _)          -> e              -- var is not poly, nothing to do
    (_, TData xhr _) -> f (concat xhr) -- xtp is concrete     -- TODO: _
    (_, TVar _)      -> f "a"          -- xtp is not concrete yet
    otherwise        -> e
    where
      f suf = ECall z (EVar z $ id++"'")
                      (fromList $ map (EVar z) $ map toID $ cs') where
                        toID id = "d" ++ id ++ suf

-- pat1::B = id2(neq) e2::(B,B)
mE globs _ dsigs xtp e@(ECall z1 e2@(EVar z2 id2) e3) = ECall z1 e2' e3 where

  recs2     = getRecDatas tp2
  (cs2,tp2) = dsigsFind dsigs id2
  cs2'      = Ifce.ifcesSups globs (getCtrs cs2) where

  e2' = case (cs2', tp2) of
    ([], _)               -> e2      -- var is not poly, nothing to do

    (_,  TFunc inp2 out2) ->
      case xhr inp2 out2 of          --   ... and xtp is concrete -> resolve!
        Left ()           -> e2
        Right (Just xhr)  -> f (concat xhr)
        Right Nothing     -> f "a"          -- xtp is not concrete yet
      where
        f suf = ECall z2 (EVar z2 $ id2++"'")
                         (fromList $ map g $ cs2') where
          g ifc = if elem suf recs2 then
                    EFunc z2 cz TAny []
                      (ExpWhere (z2, [],
                        ECase z2 (EArg z2) [
                          (PTuple z2 [PWrite z2 "k",PWrite z2 "v"],
                            ExpWhere (z2, [],
                              ETuple z2 [ECall z2 (EVar z2 "getHash")
                                          (ETuple z2 [EVar z2 ("ds_"++ifc),EVar z2 "k"]),
                                         EVar z2 "v"]))
                        ]))
                  else
                    EVar z2 $ "d" ++ ifc ++ suf

    otherwise             -> e2      -- var is not function, ignore

  xhr inp2 out2 = --traceShow (id2, toString e, toString e3, toType dsigs e3) $
    case tpMatch (TTuple [inp2            , out2])
                 (TTuple [toType dsigs e3 , xtp ]) of
      [("a", TUnit)]       -> Right $ Just ["Unit"]
      [("a", TData xhr _)] -> Right $ Just xhr   -- TODO: _
      [("a", TVar  "a")]   -> Right $ Nothing
      otherwise            -> Left ()
        where
          -- eq :: (a,a) -> Bool
          [tvar2] = toVars tp2   -- [a]
          -- a is Bool

  getRecDatas :: Type -> [ID_Var]
  getRecDatas (TData (hr:_) [(TVar "a")]) = case dataFind globs hr of
                                              Data (_,True,_,_,_) -> ["a"]
                                              otherwise           -> []
  getRecDatas (TTuple ts)                 = concatMap getRecDatas ts
  getRecDatas (TFunc inp out)             = getRecDatas inp ++ getRecDatas out
  getRecDatas _                           = []

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

toVars :: Type -> [ID_Var]
toVars tp = S.toAscList $ aux tp where
  aux TAny            = S.empty
  aux TUnit           = S.empty
  aux (TData _ [])    = S.empty
  aux (TVar id)       = S.singleton id
  aux (TFunc inp out) = S.union (aux inp) (aux out)
  aux (TTuple tps)    = S.unions (map aux tps)
