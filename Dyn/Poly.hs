module Dyn.Poly where

import Debug.Trace
import qualified Data.Set  as S
import qualified Data.Map  as M

import Dyn.AST
import Dyn.Classes
import Dyn.Map
import qualified Dyn.Ifce as Ifce

-------------------------------------------------------------------------------

apply :: [Ifce] -> [Decl] -> [Decl]
apply ifces decls = mapDecls (fDz,fE,fPz) ifces cz [] decls where

-------------------------------------------------------------------------

-- EVar:  pat::B = id(maximum)
-- ECall: pat::B = id2(neq) $ e2::(B,B)

fE :: [Ifce] -> Ctrs -> [Decl] -> Type -> Expr -> Expr

-- pat::Bool = id(maximum)
fE ifces _ dsigs xtp e@(EVar z id) = e' where

  (cs,_) = dsigsFind dsigs id
  cs'    = Ifce.ifcesSups ifces (getCtrs cs) where

  e' = case (cs', xtp) of
    ([], _)          -> e                          -- var is not poly, nothing to do
    (_, TData xhr _) -> xxx z cs' (concat xhr) id  -- xtp is concrete     -- TODO: _
    (_, TVar _)      -> xxx z cs' "a"          id  -- xtp is not concrete yet
    otherwise        -> e

-- pat1::B = id2(neq) e2::(B,B)
fE ifces _ dsigs xtp e@(ECall z1 e2@(EVar z2 id2) e3) = {-traceShow (id2,cs2,toString e2') $-} ECall z1 e2' e3 where

  (cs2,tp2) = dsigsFind dsigs id2
  cs2'      = Ifce.ifcesSups ifces (getCtrs cs2) where

  e2' = case (cs2', tp2) of
    ([], _)               -> e2      -- var is not poly, nothing to do
    (_,  TFunc inp2 out2) ->
      case xhr inp2 out2 of          --   ... and xtp is concrete -> resolve!
        Left ()           -> e2
        Right (Just xhr)  -> xxx z2 cs2' (concat xhr) id2
        Right Nothing     -> xxx z2 cs2' "a"          id2 -- xtp is not concrete yet
    otherwise             -> e2      -- var is not function, ignore

  xhr inp2 out2 = --traceShow (id2, toString e, toString e3, toType dsigs e3) $
    case tpMatch (TTuple [inp2            , out2])
                 (TTuple [toType dsigs e3 , xtp ]) of
      [("a", TData xhr _)] -> Right $ Just xhr   -- TODO: _
      [("a", TVar  "a")]   -> Right $ Nothing
      otherwise            -> Left ()
        where
          -- eq :: (a,a) -> Bool
          [tvar2] = toVars tp2   -- [a]
          -- a is Bool

fE _ _ _ _ e = e

-------------------------------------------------------------------------

-- (a,a) vs (Bool.True,Bool.False)  -> [(a,Bool)]
tpMatch :: Type -> Type -> [(ID_Var,Type)]
tpMatch ttp1 ttp2 = M.toAscList $ aux ttp1 ttp2 where
  aux :: Type -> Type -> M.Map ID_Var Type
  aux (TVar id)    TAny                    = M.singleton id TAny
  aux (TVar id)    (TData (hr:_) ofs)      = M.singleton id (TData [hr] ofs)    -- TODO: ofs
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
  aux (TData _ [])    = S.empty
  aux (TVar id)       = S.singleton id
  aux (TFunc inp out) = S.union (aux inp) (aux out)
  aux (TTuple tps)    = S.unions (map aux tps)

xxx z ifcs suf id = ECall z (EVar z $ id++"'")
                            (fromList $ map (EVar z) $ map toID $ ifcs) where
                              toID id = "d" ++ id ++ suf
