{-# LANGUAGE QuasiQuotes #-}

module Dyn.Ifce where

import Debug.Trace
import Data.Bool          (bool)
import Data.Maybe         (fromJust)
import Data.List as L     (find)
import Text.RawString.QQ

import Dyn.AST
import Dyn.Classes

-------------------------------------------------------------------------------

findIfce :: [Ifce] -> ID_Ifce -> Ifce
findIfce ifces ifc = fromJust $ L.find f ifces where
                      f :: Ifce -> Bool
                      f (Ifce (_,id,_,_)) = (id == ifc)

-- [...] -> [(a,["IOrd"])] -> ["IEq","IOrd"]
sups :: [Ifce] -> Ifce -> [ID_Ifce]
sups ifces (Ifce (_,ifc,[],     _)) = [ifc]
sups ifces (Ifce (_,ifc,[(_,l)],_)) = concatMap f l ++ [ifc] where
                                        f sup = sups ifces $ findIfce ifces sup
sups _ _ = error $ "TODO: multiple constraints"
--sups _ x = error $ "TODO: multiple constraints (" ++ show x ++ ")"

-- IEq -> [eq,neq]
ifceToIds :: Ifce -> [ID_Var]
ifceToIds (Ifce (_,_,_,dcls)) = map getId $ filter isDSig dcls where
                                  getId (DSig _ id _) = id

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- interface IEq for a
--
--  dIEq = Dict.IEq (eq,neq)          -- AUTO
--  eq = func :: ((a,a) -> Bool) ->   -- AUTO: a is IEq
--    ret where
--      <...>
--      -- AUTO
--      ... = (p1,...pN)
--      (f1,...,g1) = d1
--      (fN,...,gN) = dN
--      ((d1,...,dN), p1,...,pN) = ...

ifceToDecls :: [Ifce] -> Ifce -> [Decl]
ifceToDecls ifces me@(Ifce (z,ifc_id,_,dcls)) = dict ++ dcls' where

  -- dIEq = Dict.IEq (eq,neq)
  -- (only if all methods are implemented)
  dict :: [Decl]
  dict = bool [] [datr] has_all_impls where
          datr = DAtr z (PWrite z ("d"++ifc_id)) (Where (z,e,[])) where
            e = ECall z (ECons z ["Dict",ifc_id])
                        (fromList $ map (EVar z) $ ifceToIds me)

          has_all_impls = (length dsigs == length datrs) where
                            (dsigs, datrs) = declsSplit dcls

  dcls' = ifceImplToDecls ifces me dcls

-------------------------------------------------------------------------------

-- implemenation of IEq for Bool
-- implemenation of IEq for a where a is IXxx
--
--  dIEqBool = Dict.IEq (eq,neq) where
--    eq = func :: ((a,a) -> Bool) ->   -- AUTO: a is IEq/IXxx
--      ret where
--        <...>
--        -- AUTO
--        ... = (p1,...pN)
--        (f1,...,g1) = d1
--        (fN,...,gN) = dN
--        ((d1,...,dN), p1,...,pN) = ...

implToDecls :: [Ifce] -> Impl -> [Decl]
implToDecls ifces (Impl (z,ifc,tp,dcls)) = [dict] ++ dcls' where
  -- dIEqBool = Dict.IEq (eq,neq) where eq=...
  dict = DAtr z (PWrite z ("d"++ifc++toString' tp)) (Where (z,e,dcls')) where
          e = ECall z (ECons z ["Dict",ifc])
                      (fromList $ map (EVar z) $ ifceToIds ifce)

  toString' (Type (_, TData hr, _      )) = concat hr
  toString' (Type (_, TVar _,   [(_,l)])) = concat l

  dcls' = ifceImplToDecls ifces ifce dcls

  ifce = fromJust $ L.find h ifces where
          h :: Ifce -> Bool
          h (Ifce (_,id,_,_)) = (id == ifc)

-------------------------------------------------------------------------------

ifceImplToDecls :: [Ifce] -> Ifce -> [Decl] -> [Decl]
ifceImplToDecls ifces ifce dcls =
  map f dcls where
    f dsig@(DSig _ _ _) = dsig
    f (DAtr z1 e1
        (Where (z2,
                EFunc z3 (Type (z4,TFunc inp4 out4,cs4))  ups3 (Where (z5,e5,ds5)),
                ds2))) =

      DAtr z1 e1
        (Where (z2,
                EFunc z3 (Type (z4,TFunc inp4 out4,cs4')) ups3 (Where (z5,e5,ds5')),
                ds2))
      where
        --  a where a is (IEq,IOrd)
        -- TODO: a?
        cs4' = ("a", sups ifces ifce) : cs4

        --  <...>               -- original
        --  (f1,...,g1) = d1
        --  (fN,...,gN) = dN
        --  ... = args          -- AUTO
        --  ((d1,...,dN), args) = ...
        ds5' = ds5 ++ fsDicts5 ++ [
          DAtr z1 (PArg z1)                             (Where (z1,EVar z1 "args",[])),
          DAtr z1 (PTuple z1 [dicts5,PWrite z1 "args"]) (Where (z1,EArg z1,[]))
         ]

        -- [Dict.IEq (eq,neq) = daIEq]
        fsDicts5 :: [Decl]
        fsDicts5 = map f dicts where
          f :: (ID_Var,ID_Ifce,[ID_Var]) -> Decl
          f (var,ifc,ids) = DAtr z1 pat (Where (z1,exp,[])) where
            -- Dict.IEq (eq,neq)
            pat :: Patt
            pat = PCall z1 (PCons z1 ["Dict",ifc]) (fromList $ map (PWrite z1) ids)
            -- daIEq
            exp :: Expr
            exp = EVar z1 $ 'd':var++ifc

        -- (d1,...,dN)
        dicts5 :: Patt
        dicts5 = fromList $ map (PWrite z1) $ map (\(var,ifc,_) -> 'd':var++ifc) dicts
                                                -- [daIEq,daIShow,dbIEq,...]

        -- [ (a,IEq,[eq,neq]), (a,IOrd,[...]), (b,...,...), ...]
        dicts :: [(ID_Var,ID_Ifce,[ID_Var])]
        dicts = concatMap f cs4' where
          -- (a,[IEq,IShow]) -> [(a,IEq,[eq,neq]), (a,IOrd,[lt,gt,lte,gte]]
          f :: (ID_Var,[ID_Ifce]) -> [(ID_Var,ID_Ifce,[ID_Var])]
          f (var,ifcs) = map h $ map g ifcs where
                          h :: (ID_Ifce,[ID_Var]) -> (ID_Var,ID_Ifce,[ID_Var])
                          h (ifc,ids) = (var,ifc,ids)

          -- IEq -> (IEq, [eq,neq])
          g :: ID_Ifce -> (ID_Ifce,[ID_Var])
          g ifc = (ifc, ifceToIds $ findIfce ifces ifc)

    f decl = error $ toString decl

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

ieq = [r|
  interface IEq for a with
    eq :: ((a,a) -> Bool) = func :: ((a,a) -> Bool) ->
      case (x,y) of
        (~y,_) -> Bool.True
        _      -> Bool.False
      ; where
        (x,y) = ...
      ;
    ;
    neq :: ((a,a) -> Bool) = func :: ((a,a) -> Bool) ->
      not (eq (daIEq,(x,y))) where
        (x,y) = ...
      ;
    ;
  ;
|]

iord = [r|
  interface IOrd for a where a is IEq with
    lt  :: ((a,a) -> Bool)
    lte :: ((a,a) -> Bool)
    gt  :: ((a,a) -> Bool)
    gte :: ((a,a) -> Bool)

    lte = func :: ((a,a) -> Bool) ->
      or ( lt ((daIEq,daIOrd),(x,y)),
           eq (daIEq,(x,y)) ) where
        (x,y) = ...
      ;
    ;
    gt = func :: ((a,a) -> Bool) ->
      not (lte ((daIEq,daIOrd),(x,y))) where
        (x,y) = ...
      ;
    ;
    gte = func :: ((a,a) -> Bool) ->
      or ( gt ((daIEq,daIOrd),(x,y)),
           eq (daIEq,(x,y)) ) where
        (x,y) = ...
      ;
    ;
  ;
|]

-------------------------------------------------------------------------------

ieq_bool = [r|
  implementation of IEq for Bool with
    eq = func :: ((Bool,Bool) -> Bool) ->
      or (and (x,y), (and (not x, not y))) where
        (x,y) = ...
      ;
    ;
  ;
|]

iord_bool = [r|
  implementation of IOrd for Bool with
    lt = func :: ((Bool,Bool) -> Bool) ->
      case (x,y) of
        (Bool.False, Bool.False) -> Bool.False
        (Bool.False, Bool.True)  -> Bool.True
        (Bool.True,  Bool.False) -> Bool.False
        (Bool.True,  Bool.True)  -> Bool.False
      ; where
        (x,y) = ...
      ;
    ;
  ;
|]
