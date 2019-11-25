module Dyn.Poly where

import Debug.Trace
import Data.Bool (bool)
import qualified Data.List as L
import qualified Data.Set  as S
import qualified Data.Map  as M

import Dyn.AST
import Dyn.Classes
import Dyn.Map
import qualified Dyn.Ifce as Ifce

-------------------------------------------------------------------------------

apply :: [Glob] -> [Glob]
apply globs = mapGlobs (mSz,mDz,mWz,mPz,mE1) $
              mapGlobs (mSz,mDz,mWz,mPz,mE2) globs

              -- first map calls because they may depend on arguments that
              -- should not be mapped yet

-------------------------------------------------------------------------------

-- EVar:  pat::B = id(maximum)
-- ECall: pat::B = id2(neq) $ e2::(B,B)

mE1 :: [Glob] -> CTs -> [Decl] -> Type -> Expr -> Expr

-- pat::Bool = id(maximum)
mE1 globs cts dsigs xtp   (EVar z ('$':id)) = EVar z id
mE1 globs cts dsigs xtp e@(EVar z id) = e' where

  (dcs,dtp) = dsigsFind dsigs id
  dcs'      = Ifce.ifcesSups globs (getCtrs dcs) where

  e' = case (isVarInRec globs "a" cts, xtp, dcs', dtp) of

    -- local poly var with enclosing recursive declaration
    --    f :: (List of a -> ...) where a is IEnum
    --      v where v::a
    (Just cs, _, [], TVar "a")  -> toV "a" (getCtrs cs) e
    (Nothing, _, [], TVar "a")  -> e

    -- id is not poly, nothing to do, just keep it
    --    Bool.True
    (_, _, [], _)               -> e

    -- xtp is concrete, instantiate e with it
    --    maximum :: Bool
    --    maximum' dIBoundedBool
    (_, TData xhr2 _, _, _)
      | not (has "a" xtp)       -> toID' (concat xhr2)     -- TODO: TData ofs

    -- xtp is not concrete yet
    -- .
    --(_, TVar "a", [])    -> error $ toString e --f "a"

    otherwise                   -> e

    where
      toID' suf = ECall z (EVar z $ id++"'")
                          (fromList $ map (EVar z) $ map toID $ dcs') where
                            toID ifc = "d" ++ ifc ++ suf -- dIEqa / dIEqBool

      toV "a" [ifc] e = ECall z (EVar z "snd")
                                (ECall z (EVar z $ "d"++ifc++"a") e)

mE1 _ _ _ _ e = e

-------------------------------------------------------------------------------

-- pat1::B = id2(neq) e2::(B,B)
mE2 globs cts dsigs xtp xxx@(ECall z1 e2@(EVar z2 id2) e3) = ECall z1 e2' e3 where

  TFunc inp2 out2 = tp2
  (cs2,tp2)       = dsigsFind dsigs id2
  cs2'            = Ifce.ifcesSups globs (getCtrs cs2) where

  tp3 = toType dsigs e3

  e2' = case (cs2',
              isVarInRec globs "a" [(cs2,tp2)],
              tpMatch (TTuple [inp2, out2]) -- TODO: variance
                      (TTuple [tp3,  xtp ]),
              isVarInRec globs "a" cts)
        of

    -- isTrue Bool.True
    --  call id2 is not poly, nothing to do, just keep it
    ([], _, _, _)                    -> e2

    -- call id2 is poly

    --    function id2 receives recursive generic with constraint
    --      f :: (List of a -> ...) where a is IEnum
    --      - a is generic
    --      - List of a is recursive
    --      - a is constrained

    -- (X)
    -- f [(),T,1]
    (_, Just _, [("a", TIfce _)],     _) -> call' indirectHash
    -- (Y)
    -- f [T,F,T]
    (_, Just _, [("a", TData xhr _)], _) -> call' $ indirectConst $ concat xhr

    -- (C)
    -- f l where l::List of a (inside f)
    (_, Just _, _, Just _) -> call' $ direct "a"

    -- toNat ()
    -- eq (Bool.True,Bool.False)
    --    xtp is concrete -> resolve!
    --    (toNat' dIEnumUnit) ()
    --    (eq' dIEqBool) (Bool.True,Bool.False)
    (_, _, [("a", TUnit)], _)        -> call' $ direct $ concat ["Unit"]
    (_, _, [("a", TData xhr _)], _)  -> traceShow (id2,xhr,out2,xtp) $ call' $ direct $ concat xhr   -- TODO: _

    -- A
    -- toNat v  where v::a
    -- eq (x,y) where x::a, y::a
    --    function id2 receives generic
    --    (eq' dIEqa) (x,y)
    (_, _, [("a", TVar  "a")], Nothing) -> call' $ direct "a"

    -- B
    -- inside recursive generic with constraint
    -- toNat v where v::a
    -- (toNat' (fst (dIEnum v))) (snd (dIEnum v))
    (_, _, [("a", TVar  "a")], Just cs) -> traceShow id2 $ ECall z2 (EVar z2 $ id2++"'") (toD "a" (getCtrs cs) e3)

{-
    -- ... but xtp is not concrete yet
    (_, [("a", TVar  "a")])

      --    f :: (List of a -> ...) where a is IEnum
      --    (f' hash) l
      | elem "a" recs2            -> indirect' "a"
-}

    --(x,y) -> error $ show (toString xxx, x,y, (inp2,out2), (toType dsigs e3,xtp))

  recs2 = getRecDatas globs tp2

  call' df = ECall z2 (EVar z2 $ id2++"'") df

  direct suf = fromList $ map (EVar z2) $ map toID $ cs2' where
                toID ifc = "d" ++ ifc ++ suf -- dIEqa / dIEqBool

  indirectHash = fromList $ map f $ cs2' where
    f ifc = EFunc z2 cz TAny [] -- f' (func -> (hash (ds_IEnum k), v) where (k,v)=...
              (ExpWhere (z2, [],
                ETuple z2 [ECall z2 (EVar z2 "getHash")
                                    (ETuple z2 [EVar z2 ("ds_"++ifc),key]),
                           val])) where
              key = ECall z2 (EVar z2 "fst") (EArg z2)
              val = ECall z2 (EVar z2 "snd") (EArg z2)

  indirectConst suf = fromList $ map f $ cs2' where
    f ifc = EFunc z2 cz TAny [] -- f' (func -> (dBoolIEnum, ...)
              (ExpWhere (z2, [],
                ETuple z2 [EVar z2 ("d"++ifc++suf), EArg z2]))

  toD "a" [ifc] e = ECall z2 (EVar z2 "fst")
                             (ECall z2 (EVar z2 $ "d"++ifc++"a") e') where
                      e' = mapExpr (mSz,mDz,mWz,mPz,mE) globs cts dsigs TAny e where
                            mE _ _ _ _ (EVar z id) = (EVar z ("$"++id))   -- TODO
                            mE _ _ _ _ e           = e

--    l :: List of IEnum;
--    l = List.Cons (Bool.True,
--        List.Cons ((),
--        List.Nil));
--
--    l :: List of IEnum;
--    l = List.Cons ((Key.Bool, Bool.True),
--        List.Cons ((Key.Unit, ()),
--        List.Nil));
mE2 globs cts dsigs xtp e@(ECall z1 e2@(ECons z2 (hr2:_)) e3)
  | hasIfce xtp = ECall z1 e2 e3'
  where
    ETuple _ es3 = e3
    e3' = ETuple z2 $ map f $ zip tps es3 where
            TData hr _ = toType dsigs e2
            Data (_,_,_,_,TTuple tps) = dataFind globs hr

    f (TVar "a", e) = ETuple z2 [key,e] where
                        key = ECons z2 ["Key", tpToString' $ toType dsigs e]
    f (_,        e) = e

    -- TODO: tirar isso // o xtp que ser List of IEnum ou List of Bool
    --isRecGen = traceShowId (getRecDatas globs xtp == ["a"])

    tpToString' (TData hr []) = head hr
    tpToString' TUnit         = "Unit"
    tpToString' x = error $ show (toString e, cts,xtp, x)

--mE2 globs cts dsigs xtp e@(ECall z1 e2@(ECons z2 (hr2:_)) e3) = traceShowS e

mE2 _ _ _ _ e = e

-------------------------------------------------------------------------

-- (a,a) vs (Bool.True,Bool.False)  -> [(a,Bool)]
tpMatch :: Type -> Type -> [(ID_Var,Type)]
tpMatch tp1 tp2 = {-traceShow ("MATCH",toString tp1,toString tp2) $-} M.toAscList $ aux tp1 tp2 where
  aux :: Type -> Type -> M.Map ID_Var Type
  aux (TVar id)      TAny                    = M.singleton id TAny
  aux (TVar id)      TUnit                   = M.singleton id TUnit
  aux (TVar id)      (TData (hr:_) ofs)      = M.singleton id (TData [hr] ofs)    -- TODO: ofs
  aux (TVar id)      (TIfce ifcs)            = M.singleton id (TIfce ifcs)
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
getRecDatas globs (TData (hr:_) [(TVar "a")]) = case dataFind globs [hr] of
                                                  Data (_,True,_,_,_) -> ["a"]
                                                  otherwise           -> []
getRecDatas globs (TTuple ts)                 = concatMap (getRecDatas globs) ts
getRecDatas globs (TFunc inp out)             = getRecDatas globs inp ++ getRecDatas globs out
getRecDatas _ _                               = []

-------------------------------------------------------------------------------

isVarInRec :: [Glob] -> ID_Var -> CTs -> Maybe Ctrs
isVarInRec globs "a" cts =
  case L.find ((has "a") . snd) cts of
    Nothing      -> Nothing
    Just (cs,tp) -> bool Nothing (Just cs) $ any isRec $ datasWithVar "a" tp
  where
    isRec tp = elem "a" (getRecDatas globs tp)

datasWithVar :: ID_Var -> Type -> [Type]
datasWithVar  _  TAny             = []
datasWithVar  _  TUnit            = []
datasWithVar  _  (TVar _)         = []
datasWithVar "a" tp@(TData _ ofs) = bool [] [tp] (any (has "a") ofs)
datasWithVar "a" (TTuple tps)     = concatMap (datasWithVar "a") tps
datasWithVar "a" (TFunc inp out)  = datasWithVar "a" inp ++ datasWithVar "a" out

has :: ID_Var -> Type -> Bool
has "a" TAny            = False
has "a" TUnit           = False
has "a" (TVar x)        = ("a" == x)
has "a" (TData _ ofs)   = or $ map (has "a") ofs
has "a" (TTuple tps)    = any (has "a") tps
has "a" (TFunc inp out) = has "a" inp || has "a" out

hasIfce :: Type -> Bool
hasIfce TAny            = False
hasIfce TUnit           = False
hasIfce (TVar x)        = False
hasIfce (TData _ ofs)   = or $ map hasIfce ofs
hasIfce (TIfce _)       = True
hasIfce (TTuple tps)    = any hasIfce tps
hasIfce (TFunc inp out) = hasIfce inp || hasIfce out
