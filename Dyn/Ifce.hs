module Dyn.Ifce where

import Debug.Trace
import Data.Bool                (bool)
import Data.Maybe               (fromJust)
import qualified Data.List as L (find, sort)

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

inline :: [Ifce] -> [Glob] -> [Decl]
inline ifces globs = concatMap globToDecl globs where

  globToDecl :: Glob -> [Decl]
  globToDecl (GDecl dcl) = declToDecls ifces dcl
  globToDecl (GIfce ifc) = ifceToDecls ifces ifc
  globToDecl (GImpl imp) = implToDecls ifces imp

  declToDecls ifces dcl = mapDecl (fD,fEz,fPz) ifces [] dcl where

    -- f :: ((a,a) -> Bool) where a is IOrd

    fD :: [Ifce] -> [Decl] -> Decl -> [Decl]

    fD ifces dsigs d@(DSig z1 id1 (Type (z2,ttp2,cs))) =
      case cs of
        []            -> [d]   -- no constraints on declaration
        [("a",[ifc])] -> [expandDecl ifces False (ifc,[]) (DSig z1 id1 (Type (z2,ttp2,[])))]
        otherwise     -> error $ "TODO: multiple vars/ctrs"

    fD ifces dsigs d@(DAtr z pat@(PWrite _ id) whe) =
      case dsigFind dsigs id of
        Type (_,_,[])            -> [d]   -- no constraints on declaration
        Type (_,_,[("a",[ifc])]) -> [expandDecl ifces False (ifc,[]) d]
        otherwise                -> error $ "TODO: multiple vars/ctrs"

    fD _ _ d = [d]

-------------------------------------------------------------------------------

-- interface IEq for a
--  dIEq = Dict.IEq (eq,neq) -- : declare instance dict if all defaults are implemented
--  <...>                    -- : modify nested impls which become globals

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

  decls' = map (expandDecl ifces False (ifc_id,[])) decls

-------------------------------------------------------------------------------

-- implemenation of IEq for Bool
-- implemenation of IEq for a where a is IXxx
--  dIEqBool = Dict.IEq (eq,neq) where    -- : declare instance dict with methods
--              <...>                     -- :   with nested impls to follow only visible here

implToDecls :: [Ifce] -> Impl -> [Decl]
implToDecls ifces (Impl (z,ifc,tp@(Type (_,_,cs)),decls)) = [dict] where

  -- dIEqBool = func -> Dict.IEq (eq,neq) where eq=<...> daIXxx=...;
  -- func b/c of HKT that needs a closure with parametric dictionary
  dict = DAtr z (PWrite z ("d"++toString' tp++ifc)) (ExpWhere (z,f,[])) where
          f = EFunc z tz [] (ExpWhere (z,d,decls'++[ups']))
          d = ECall z (ECons z ["Dict",ifc])
                      (fromList $ map (EVar z) $ ifceToDeclIds ifce)

  -- {daIXxx} // implementation of IOrd for a where a is IXxx
  ups' = DAtr z (fromList $ map (PWrite z) $ L.sort $ map ("da"++) imp_ids)
                (ExpWhere (z,EArg z,[]))

  toString' (Type (_, TData hr, _      )) = concat hr
  toString' (Type (_, TVar _,   [(_,l)])) = concat l

  -- eq = <...>
  decls' = map (expandDecl ifces True (id,imp_ids)) decls where
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
-- Bool:                is implementation (or interface)
-- ([ID_ifce],[ID_Ifce]): ifce constraint // impl extra constraints
--                          - ifce method always has ifce but not impl constraints
--                          - impl method always has ifce and maybe impl constraints
--                          - gnrc method never has ifce or impl constraints
--                              - it is already on its type explicitly
-- Decl:                decl to expand
-- Decl:                expanded decl
expandDecl :: [Ifce] -> Bool -> (ID_Ifce,[ID_Ifce]) -> Decl -> Decl

expandDecl ifces True _ (DSig z id _) = DSig z id tz -- isImpl: prevent clashes w/ poly
expandDecl ifces False (ifc_id,imp_ids) (DSig z1 id1 (Type (z2,ttp2,cs2))) =
  DSig z1 id1 (Type (z2,ttp2,cs2')) where
    -- TODO: a?
    cs2' = ("a", ifcesSups ifces (ifc_id:imp_ids)) : cs2

-- IBounded: minimum/maximum
expandDecl _ _ _ decl@(DAtr _ _ (ExpWhere (_,econst,_))) | isConst econst = decl where
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
expandDecl ifces _ (ifc_id,imp_ids)
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

expandDecl _ _ _ decl = error $ toString decl
