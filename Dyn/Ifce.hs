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
                        f (Ifce (_,ifc,Ctrs [],     _)) = []
                        f (Ifce (_,ifc,Ctrs [(_,l)],_)) = l
                        f _ = error $ "TODO: multiple constraints"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

inline :: [Ifce] -> [Impl] -> [Glob] -> [Decl]
inline ifces impls globs = concatMap globToDecl globs where

  globToDecl :: Glob -> [Decl]
  globToDecl (GDecl dcl) = mapDecl (fD,fEz,fPz) ifces cz [] dcl
  globToDecl (GIfce ifc) = ifceToDecls ifces ifc
  globToDecl (GImpl imp) = implToDecls ifces impls imp

-------------------------------------------------------------------------------

-- interface IEq for a
--  dIEq = Dict.IEq (eq,neq) -- : declare instance dict if all defaults are implemented
--  <...>                    -- : modify nested impls which become globals

ifceToDecls :: [Ifce] -> Ifce -> [Decl]
ifceToDecls ifces me@(Ifce (z,ifc_id,ctrs,decls)) = dict ++ decls' where

  -- dIEq = Dict.IEq (eq,neq)
  -- (only if all methods are implemented)
  dict :: [Decl]
  dict = bool [] [datr] has_all_impls where
          datr = DAtr z (PWrite z ("d"++ifc_id)) (ExpWhere (z,f,[])) where
            f = EFunc z cz TAny [] (ExpWhere (z,d,[]))
            d = ECall z (ECons z ["Dict",ifc_id])
                        (fromList $ map (EVar z) $ ifceToDeclIds me)

          has_all_impls = (length dsigs == length datrs) where
                            (dsigs, datrs) = declsSplit decls

  decls' = mapDecl (fD,fEz,fPz) ifces ctrs' [] dcl where
            ctrs' = Ctrs $ ("a", ifcesSups ifces [ifc_id])         : cs3

            .

-------------------------------------------------------------------------------

-- implemenation of IEq for Bool
-- implemenation of IEq for a where a is IXxx
--  dIEqBool = Dict.IEq (eq,neq) where    -- : declare instance dict with methods
--              <...>                     -- :   with nested impls to follow only visible here

implToDecls :: [Ifce] -> [Impl] -> Impl -> [Decl]
implToDecls ifces impls (Impl (z,ifc,cs,tp,decls)) = ctrDicts++[dict] where

  -- implementation of IOrd for a where a is IAaa with
  -- implementation of IAaa for Xxx with
  -- find all implementations of IAaa ([IXxx,...])
  ctrDicts :: [Decl]
  ctrDicts = map toDict $ map getTp $ concatMap findImpls imp_ids

    -- find all implementations of IAaa
    findImpls :: ID_Ifce -> [Impl]
    findImpls ifc1 = filter isSame impls where  -- IAaa == IAaa
                      isSame (Impl (_,ifc2,_,_,_)) = (ifc1 == ifc2)

    getTp (Impl (_,_,_,tp,_)) = tp              -- Xxx

    -- dXxxIOrd = dIAaaIOrd dXxxIAaa
    -- tp = Xxx
    toDict :: Type -> Decl
    toDict tp = DAtr z (PWrite z ("d"++toString' tp++ifc)) (ExpWhere (z,e,[])) where
                  e = ECall z (EVar z $ "d"++head ctr++ifc)
                        (EVar z $ "d"++toString' tp++head ctr)

  -- dIEqBool = Dict.IEq (eq,neq) where eq=<...> daIXxx=... ;
  dict = DAtr z (PWrite z ("d"++toString' tp++ifc)) d where
          d = case imp_ids of
                []        -> ExpWhere (z,d1,decls')
                otherwise -> ExpWhere (z,d2,[])

          d1 = ECall z (ECons z ["Dict",ifc])
                       (fromList $ map (EVar z) $ ifceToDeclIds ifce)

          -- func b/c needs a closure with parametric dictionary
          d2 = EFunc z cz TAny [] $ ExpWhere (z,d1,decls'++[ups'])

          -- {daIXxx} // implementation of IOrd for a where a is IXxx
          ups' = DAtr z (fromList $ map (PWrite z) $ L.sort $ map ("da"++) imp_ids)
                    (ExpWhere (z,EArg z,[]))

  toString' (TData hr) = concat hr
  toString' (TVar _)   = concat l where [(_,l)]=cs

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

-- [Ifce]:              known interfaces
-- ([ID_ifce],[ID_Ifce]): ifce constraint // impl extra constraints
--                          - ifce method always has ifce but not impl constraints
--                          - impl method always has ifce and maybe impl constraints
--                          - gnrc method never has ifce or impl constraints
--                              - it is already on its type explicitly
-- Decl:                decl to expand
-- Decl:                expanded decl
expandDecl :: [Ifce] -> (ID_Ifce,[ID_Ifce]) -> Decl -> Decl

expandDecl ifces _ (DSig z id _) = DSig z id TAny -- isImpl: prevent clashes w/ poly

-- IBounded: minimum/maximum
expandDecl _ _ decl@(DAtr _ _ (ExpWhere (_,econst,_))) | isConst econst = decl where
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
expandDecl ifces (ifc_id,imp_ids)
           (DAtr z1 e1
            (ExpWhere (z2,
              EFunc z3 (Ctrs cs3) (TFunc inp4 out4) [] (ExpWhere (z5,e5,ds5)),
              ds2))) =
  DAtr z1 e1
    (ExpWhere (z2,
               EFunc z3 cs3NImps' (TFunc inp4 out4) ups3' (ExpWhere (z5,e5,ds5')),
               ds2))
  where
    --  a where a is (IEq,IOrd)
    -- TODO: a?
    -- TODO: ctrsUnion
    cs3NImps' = Ctrs $ ("a", ifcesSups ifces [ifc_id])         : cs3
    cs3YImps  = Ctrs $ ("a", ifcesSups ifces (ifc_id:imp_ids)) : cs3

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
    fsDicts5 = map f (dicts cs3YImps) where
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
    dicts5 = fromList $ map (PWrite z1) $ L.sort $ map (\(var,ifc,_) -> 'd':var++ifc) (dicts cs3NImps')
                                                  -- [daIEq,daIShow,dbIEq,...]

    -- [ (a,IEq,[eq,neq]), (a,IOrd,[...]), (b,...,...), ...]
    dicts :: Ctrs -> [(ID_Var,ID_Ifce,[ID_Var])]
    dicts (Ctrs cs) = concatMap f cs where
      -- (a,[IEq,IShow]) -> [(a,IEq,[eq,neq]), (a,IOrd,[lt,gt,lte,gte]]
      f :: (ID_Var,[ID_Ifce]) -> [(ID_Var,ID_Ifce,[ID_Var])]
      f (var,ifcs) = map h $ map g ifcs where
                      h :: (ID_Ifce,[ID_Var]) -> (ID_Var,ID_Ifce,[ID_Var])
                      h (ifc,ids) = (var,ifc,ids)

      -- IEq -> (IEq, [eq,neq])
      g :: ID_Ifce -> (ID_Ifce,[ID_Var])
      g ifc = (ifc, ifceToDeclIds $ ifceFind ifces ifc)

expandDecl _ _ decl = error $ toString decl
