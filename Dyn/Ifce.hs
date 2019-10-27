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
sups :: [Ifce] -> [ID_Ifce] -> [ID_Ifce]
sups ifces []  = []
sups ifces ids = sups ifces ids' ++ ids where
                  ids' = concatMap (f . (findIfce ifces)) ids
                  f (Ifce (_,ifc,[],     _)) = []
                  f (Ifce (_,ifc,[(_,l)],_)) = l
                  f _ = error $ "TODO: multiple constraints"

-- IEq -> [eq,neq]
ifceToIds :: Ifce -> [ID_Var]
ifceToIds (Ifce (_,_,_,dcls)) = map getId $ filter isDSig dcls where
                                  getId (DSig _ id _) = id

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
          datr = DAtr z (PWrite z ("d"++ifc_id)) (Where (z,f,[])) where
            f = EFunc z tz [] (Where (z,d,[]))
            d = ECall z (ECons z ["Dict",ifc_id])
                        (fromList $ map (EVar z) $ ifceToIds me)

          has_all_impls = (length dsigs == length datrs) where
                            (dsigs, datrs) = declsSplit decls

  decls' = map (expandDecl ifces (id,[])) decls where
            Ifce (_,id,_,_) = me

-------------------------------------------------------------------------------

-- implemenation of IEq for Bool
-- implemenation of IEq for a where a is IXxx
--  dIEqBool = Dict.IEq (eq,neq) where    -- : declare instance dict with methods
--              <...>                     -- :   with nested impls to follow only visible here

implToDecls :: [Ifce] -> Impl -> [Decl]
implToDecls ifces (Impl (z,ifc,tp@(Type (_,_,cs)),decls)) = [dict] where

  -- dIEqBool = func -> Dict.IEq (eq,neq) where eq=<...> daIXxx=...;
  -- func b/c of HKT that needs a closure with parametric dictionary
  dict = DAtr z (PWrite z ("d"++ifc++toString' tp)) (Where (z,f,[])) where
          f = EFunc z tz [] (Where (z,d,decls'++[ups']))
          d = ECall z (ECons z ["Dict",ifc])
                      (fromList $ map (EVar z) $ ifceToIds ifce)

  -- {daIXxx} // implementation of IOrd for a where a is IXxx
  ups' = DAtr z (fromList $ map (\id -> PWrite z ("da"++id)) imp_ids)
                (Where (z,EArg z,[]))

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

--  eq = func :: ((a,a) -> Bool) ->       -- : insert a is IEq/IXxx
--    ret where
--      <...>
--      ... = (p1,...pN)                  -- : restore original args
--      (fN,...,gN) = dN                  -- : restore iface functions from dicts
--      ((d1,...,dN), (p1,...,pN)) = ...  -- : split dicts / args from instance call

expandDecl :: [Ifce] -> (ID_Ifce,[ID_Ifce]) -> Decl -> Decl
expandDecl _ _ dsig@(DSig _ _ _) = dsig
expandDecl ifces
           (ifc_id,imp_ids)
           (DAtr z1 e1
            (Where (z2,
              EFunc z3 (Type (z4,TFunc inp4 out4,cs4)) [] (Where (z5,e5,ds5)),
              ds2))) =
  DAtr z1 e1
    (Where (z2,
            EFunc z3 (Type (z4,TFunc inp4 out4,cs4NImps')) ups3' (Where (z5,e5,ds5')),
            ds2))
  where
    --  a where a is (IEq,IOrd)
    -- TODO: a?
    cs4NImps' = ("a", sups ifces [ifc_id])         : cs4
    cs4YImps  = ("a", sups ifces (ifc_id:imp_ids)) : cs4

    -- {daIXxx} // implementation of IOrd for a where a is IXxx
    ups3' = map (\id -> ("da"++id,EUnit az)) imp_ids

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
    fsDicts5 = map f (dicts cs4YImps) where
      f :: (ID_Var,ID_Ifce,[ID_Var]) -> Decl
      f (var,ifc,ids) = DAtr z1 pat (Where (z1,exp,[])) where
        -- Dict.IEq (eq,neq)
        pat :: Patt
        pat = PCall z1 (PCons z1 ["Dict",ifc]) (fromList $ map (PWrite z1) ids)
        -- daIEq
        exp :: Expr
        exp = EVar z1 $ 'd':var++ifc

    -- (d1,...,dN)
    -- csNImps: exclude implementation constraints since dicts come from closure
    dicts5 :: Patt
    dicts5 = fromList $ map (PWrite z1) $ map (\(var,ifc,_) -> 'd':var++ifc) (dicts cs4NImps')
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
      g ifc = (ifc, ifceToIds $ findIfce ifces ifc)

expandDecl _ _ decl = error $ toString decl
