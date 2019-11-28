module Dyn.Ifce (apply,ifceFind,ifceToDeclIds,ifcesSups) where

import Debug.Trace
import Data.Bool                (bool)
import qualified Data.List as L (find, sort, sortBy, groupBy)

import Dyn.AST
import Dyn.Classes
import qualified Dyn.Parser as P
import qualified Dyn.Eval   as E

-------------------------------------------------------------------------------

apply :: Prog -> Prog
apply globs = datas globs ++ impls globs ++ expand globs

expand :: [Glob] -> [Glob]
expand globs = filter (not.isGDecl) globs ++ (map globFromDecl $ concatMap f globs) where
                f :: Glob -> [Decl]  -- [Decl] w/o Ifce/Impl/Gens
                f (GDecl dcl) = expandGen globs dcl
                f (GData dat) = []
                f (GIfce ifc) = ifceToDecls ifc
                f (GImpl imp) = []

datas :: [Glob] -> [Glob]
datas globs = map globFromData $ (dict : (map f $ globsToIfces globs)) where
                f :: Ifce -> Data
                f (Ifce (z,id,_,_)) = Data (z, False, ["Dict",id], [], TAny)
                dict = Data (pz, False, ["Dict"], [], TAny)

impls :: [Glob] -> [Glob]
impls globs = map globFromDecl $ dicts ++ decls where
  is = globsToImpls globs

  decls :: [Decl]
  decls = concatMap (implToDecls globs) supers

  -- group subtypes to create supertype fs:
  --  toStringExpr = toStringExprUnit + toStringExprVar + ...
  supers :: [Impl]
  supers =
    map join       $
    --map (map (\(x,y,_) -> (x,y))) $
    L.groupBy same $ -- [ [(IString,Expr.Unit),(IString,Expr.Var)], [(IString,Bool)] ]
    L.sortBy  cmp  $
    map toTriple   $ -- [ (IString,Expr.Unit), (IString,Bool), (IString,Expr.Var) ]
    is where
      toTriple impl@(Impl (_,ifc,_,tp,_)) = (ifc,tp,impl)

      cmp  (ifc1,tp1,_) (ifc2,tp2,_) = case compare ifc1 ifc2 of
                                        EQ -> compare (toString tp1) (toString tp2)
                                        x  -> x

      same (ifc1,tp1,_) (ifc2,tp2,_) = (ifc1 == ifc2) && (tp1 `f` tp2) where
        f (TData hr1 _) (TData hr2 _) = (head hr1 == head hr2)
        f tp1           tp2           = (tp1      == tp2)

  join :: [(ID_Ifce,Type,Impl)] -> Impl
  join [(_,_,x)] = x
  join l@((ifc, TData (hr:_) [], Impl (z,_,Ctrs [],_,ds)) : _) =
    Impl (z, ifc, Ctrs [], TData [hr] [], fs' ++ dss') where

      -- function to declare and wrap dss'
      fs' :: [Decl]
      fs' = map f $ filter isDAtr ds where
              f :: Decl -> Decl
              f (DAtr z pat@(PWrite _ id) _) = DAtr z pat $
                                  ExpWhere (z, [],
                                    EFunc z cz TAny ups $
                                      ExpWhere (z,[],wrap)) where
                ups  = map f dss where
                        f :: (ID_Hier,[Decl]) -> (ID_Var,Expr)
                        f (hr2,_) = (id ++ concat hr2, EUnit z)
                wrap = ECase z (EArg z) (map f dss) where
                        f :: (ID_Hier,[Decl]) -> (Patt,ExpWhere)
                        f (hr2,_) = (PCall z (PCons z (hr:hr2)) (PAny z),
                                     ExpWhere (z, [],
                                      ECall z (EVar z (id ++ concat hr2)) (EArg z)))

      dss :: [(ID_Hier,[Decl])]
      dss = map f l where
              f (_,_,Impl (_,_,_,TData (_:hr) _,ds)) = (hr,ds)

      dss' :: [Decl]
      dss' = concatMap f dss where
              f :: (ID_Hier,[Decl]) -> [Decl]
              f (hr,ds) = map rename $ filter isDAtr ds where
                            rename :: Decl -> Decl
                            rename (DAtr z1 (PWrite z2 id2) e) = DAtr z1 (PWrite z2 (id2++concat hr)) e

  dicts :: [Decl]
  dicts = --traceShowSS $
    map toDict     $   -- [ ds_IEnum=..., ... ]
    map toCons     $   -- [ (IEnum, Cons((K.Unit,dIEnumUnit), Cons(..., Nil))) ]
    L.groupBy same $   -- [ [(IEnum,...),(IEnum,...)], [(IEq,...)] ]
    L.sortBy  comp $
    map toPair     $   -- [ (IEnum, Cons(K.Unit,dIEnumUnit)), (IEnum, Cons(K.Bool,dIEnumBool), ...]
    supers where

      toDict :: (ID_Ifce,Expr) -> Decl
      toDict (ifc,cons) = DAtr pz (PWrite pz ("ds_"++ifc))
                           (ExpWhere (pz,[], cons))

      toCons :: [(ID_Ifce,Expr)] -> (ID_Ifce,Expr)
      toCons l = (fst $ head l,
                  foldr f (ECons pz ["List","Nil"]) $ map snd l)
                 where
                  f tup acc = ECall pz (ECons pz ["List","Cons"])
                                       (ETuple pz [tup,acc])

      toPair (Impl (_,ifc,_,tp,_)) = (ifc,tup) where
                                        tp' = tpToString' tp
                                        tup = ETuple pz [c1,c2] where
                                                c1 = ECons pz ["Key",tp']
                                                c2 = EVar  pz ("d"++ifc++tp')
      comp (ifc1,_) (ifc2,_) = compare ifc1 ifc2
      same (ifc1,_) (ifc2,_) = (ifc1 == ifc2)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- IEq -> [eq,neq]
ifceToDeclIds :: Ifce -> [ID_Var]
ifceToDeclIds (Ifce (_,_,_,dcls)) = map getId $ filter isDSig dcls where
                                      getId (DSig _ id _ _) = id

-- [...] -> ["IEq"] -> ["IEq","IOrd"] -- (sorted)
ifcesSups :: [Glob] -> [ID_Ifce] -> [ID_Ifce]
ifcesSups _     []  = []
ifcesSups globs ids = L.sort $ ifcesSups globs ids' ++ ids where
                        ids' = concatMap (f . (ifceFind globs)) ids
                        f (Ifce (_,_,Ctrs l,_)) = l

tpToString' (TData hr [])  = concat hr
--tpToString' (TData hr tps) = concat hr ++ concatMap tpToString' tps
--tpToString' (TVar _)       = concat cs
tpToString' TUnit          = "Unit"
tpToString' x = error $ show x

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

--  interface IEq for a
--    eq :: (a,a) -> Bool                       -- only abstract methods
--
-- expand to
--
--  data Dict.IEq (typeof(eq))
--  eq' = func -> fst ...                       -- same as below
--  eq' = func -> eq where Dict.IEq (eq) = ...  -- same as above
--  eq :: (a,a) -> Bool where a is IEq          -- global prototype (include IEq)

ifceToDecls :: Ifce -> [Decl]
ifceToDecls me@(Ifce (z,ifc_id,ctrs,decls)) = wraps ++ decls' where

  -- Same for constant (minimum) and function (eq).
  -- eq' = func -> eq where Dict.IEq (eq) = ...
  wraps = map toWrap meIds where
    toWrap id = DAtr z (PWrite z (id++"'"))
                       (ExpWhere (z,[],
                          EFunc z cz TAny [] (ExpWhere (z,[dict],EVar z id))))
     where
      dict = DAtr z pats exp where
              pats = fromList $ map f $ L.sort $ ifc_id : getCtrs ctrs where
                      f id | (id==ifc_id) = patt
                           | otherwise    = PAny z
              patt = PCall z (PCons z ["Dict",ifc_id])
                             (fromList $ map f meIds) where
                      f x = bool (PAny z) (PWrite z id) (id==x)
              exp  = ExpWhere (z,[],EArg z)

  meIds = ifceToDeclIds me

  -- include ":: where a is IEq"
  decls' = map f decls where
            f (DSig z id (Ctrs []) tp) = DSig z id (Ctrs [ifc_id]) tp

-------------------------------------------------------------------------------

-- implemenation of IEq for Bool
-- implemenation of IEq for a where a is IXxx
--  dIEqBool = Dict.IEq (eq) where    -- : declare instance dict with methods
--              <...>                 -- :   with nested impls to follow only visible here

implToDecls :: [Glob] -> Impl -> [Decl]
implToDecls globs (Impl (z,ifc,Ctrs [],tp,decls)) =
  [DAtr z (PWrite z ("d"++ifc++tpToString' tp))
          (ExpWhere (z,decls,
            ECall z (ECons z ["Dict",ifc])
                    (fromList $ map (EVar z) $ ifceToDeclIds $ ifceFind globs ifc)))]

-------------------------------------------------------------------------------

expandGen :: [Glob] -> Decl -> [Decl]

--  neq = func :: ((a,a) -> Bool) where a is IEq ->
--    not (eq ...)
--
--    expand to
--
--  neq :: ((a,a) -> Bool) where a is IEq
--  neq' = func ->
--    let dIEqa = ... in              -- receives dict
--      func :: <tp/cs> {daIEq} ->    -- same as above but as a closure with fixed dict
--        not (eq ...)                -- Poly.hs will then translate to ((eq' daIEq) ...)

expandGen globs (DAtr z1 (PWrite pz pid)
                  (ExpWhere (z2, [],
                    EFunc z3 (Ctrs cs3) tp3 [] whe3))) | not (null cs3) =
  [
    DSig z1 pid (Ctrs cs3) tp3,                   -- neq ::
    DAtr z1 (PWrite pz (pid++"'"))                -- neq' =
    (ExpWhere (z2, [],
      EFunc z2 cz TAny [] $
        ExpWhere (z2, [letDicts],            -- let dIEqa = ...
          EFunc z3 (Ctrs cs3') tp3 ups3' whe3)))
  ]
  where
    -- { dIEqa }
    ups3'    = map (\id -> ("d"++id++"a",EUnit z3)) $ cs3'
    letDicts = DAtr z2  -- (dIEqa,...) = ...
                (fromList $ map (\id -> PWrite z2 $ "d"++id++"a") cs3')
                (ExpWhere (z2,[],EArg z2))
    cs3'     = ifcesSups globs cs3

expandGen _ decl = [decl]
