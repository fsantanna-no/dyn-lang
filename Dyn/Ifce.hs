module Dyn.Ifce (apply,evalString,parseToString,ifceFind,ifceToDeclIds,ifcesSups) where

import Debug.Trace
import Data.Bool                (bool)
import Data.Maybe               (fromJust)
import qualified Data.List as L (find, sort)

import Dyn.AST
import Dyn.Classes
import qualified Dyn.Parser as P
import qualified Dyn.Eval   as E

-------------------------------------------------------------------------------

evalString :: String -> String
evalString input = E.evalStringF f input where
                    f prog = prog' where (_,prog') = apply prog

parseToString :: String -> String
parseToString input = P.parseToStringF f input where
                        f prog = prog' where (_,prog') = apply prog

-------------------------------------------------------------------------------

apply :: Prog -> ([Ifce], Prog)
apply (Prog globs) = (ifces,prog) where
  prog = Prog $
          map globFromDecl   $
          inline ifces impls $  -- [Decl] w/o Ifce/Impl/Gens
          globs

  ifces :: [Ifce]
  ifces = globsToIfces globs
  impls :: [Impl]
  impls = globsToImpls globs

  globsToIfces :: [Glob] -> [Ifce]
  globsToIfces globs = map g $ filter f globs where
                        f (GIfce ifc) = True
                        f _           = False
                        g (GIfce ifc) = ifc

  globsToImpls :: [Glob] -> [Impl]
  globsToImpls globs = map g $ filter f globs where
                        f (GImpl ifc) = True
                        f _           = False
                        g (GImpl ifc) = ifc

  inline :: [Ifce] -> [Impl] -> [Glob] -> [Decl]
  inline ifces impls globs = concatMap f globs where
                              f :: Glob -> [Decl]
                              f (GDecl dcl) = expandDecl  ifces cz dcl
                              f (GIfce ifc) = ifceToDecls ifces ifc
                              f (GImpl imp) = implToDecls ifces impls imp

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

ifceFind :: [Ifce] -> ID_Ifce -> Ifce
ifceFind ifces ifc = fromJust $ L.find f ifces where
                      f :: Ifce -> Bool
                      f (Ifce (_,id,_,_)) = (id == ifc)

-- IEq -> [eq,neq]
ifceToDeclIds :: Ifce -> [ID_Var]
ifceToDeclIds (Ifce (_,_,_,dcls)) = map getId $ filter isDSig dcls where
                                      getId (DSig _ id _ _) = id

-- [...] -> ["IEq"] -> ["IEq","IOrd"] -- (sorted)
ifcesSups :: [Ifce] -> [ID_Ifce] -> [ID_Ifce]
ifcesSups ifces []  = []
ifcesSups ifces ids = L.sort $ ifcesSups ifces ids' ++ ids where
                        ids' = concatMap (f . (ifceFind ifces)) ids
                        f (Ifce (_,_,Ctrs l,_)) = l
{-
                        f (Ifce (_,ifc,Ctrs [],_)) = []
                        f (Ifce (_,ifc,Ctrs l, _)) = l
                        f _ = error $ "TODO: multiple constraints"
-}

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- interface IEq for a
--  eq' = func -> eq ... where Dict.IEq (eq,_) = ...
--  dIEq = Dict.IEq (eq,neq) -- : declare instance dict if all defaults are implemented
--  <...>                    -- : modify nested impls which become globals

ifceToDecls :: [Ifce] -> Ifce -> [Decl]
ifceToDecls ifces me@(Ifce (z,ifc_id,ctrs,decls)) = wraps ++ dict ++ decls' where

  -- Same for constant (minimum) and function (eq).
  -- eq' = func -> eq ... where Dict.IEq (eq, _) = ...
  wraps = traceShowSS $ map toWrap meIds where
    toWrap id = DAtr z (PWrite z (id++"'")) (ExpWhere (z,func,[])) where
      func = EFunc z cz TAny [] (ExpWhere (z,call,[dict]))
      call = ECall z (EVar z id) (EArg z)   -- eq ...
      dict = DAtr z patt exp where
              patt = PCall z (PCons z ["Dict",ifc_id])
                             (PTuple z $ map f meIds) where
                      f x = bool (PAny z) (PWrite z id) (id==x)
              exp  = ExpWhere (z,EArg z,[])

  -- dIEq = Dict.IEq (eq,neq)
  -- (only if all methods are implemented)
  dict :: [Decl]
  dict = bool [] [datr] has_all_impls where
          datr = DAtr z (PWrite z ("d"++ifc_id)) (ExpWhere (z,f,[])) where
            f = EFunc z cz TAny [] (ExpWhere (z,d,[]))
            d = ECall z (ECons z ["Dict",ifc_id])
                        (fromList $ map (EVar z) meIds)

          has_all_impls = (length dsigs == length datrs) where
                            (dsigs, datrs) = declsSplit decls

          declsSplit :: [Decl] -> ([Decl],[Decl])
          declsSplit decls = (filter isDSig decls, filter isDAtr decls)

  decls' = concatMap (expandDecl ifces (Ctrs [ifc_id])) decls

  meIds = ifceToDeclIds me

-------------------------------------------------------------------------------

-- implemenation of IEq for Bool
-- implemenation of IEq for a where a is IXxx
--  dIEqBool = Dict.IEq (eq,neq) where    -- : declare instance dict with methods
--              <...>                     -- :   with nested impls to follow only visible here

implToDecls :: [Ifce] -> [Impl] -> Impl -> [Decl]
implToDecls ifces impls (Impl (z,ifc,Ctrs cs,tp,decls)) = ctrDicts++[dict] where

  -- implementation of IOrd for a where a is IAaa with
  -- implementation of IAaa for Xxx with
  -- find all implementations of IAaa ([IXxx,...])
  ctrDicts :: [Decl]
  ctrDicts = map toDict $ map getTp $ concatMap findImpls cs where

    -- find all implementations of IAaa
    findImpls :: ID_Ifce -> [Impl]
    findImpls ifc1 = filter isSame impls where  -- IAaa == IAaa
                      isSame (Impl (_,ifc2,_,_,_)) = (ifc1 == ifc2)

    getTp (Impl (_,_,_,tp,_)) = tp              -- Xxx

    -- dXxxIOrd = dIAaaIOrd dXxxIAaa
    -- tp = Xxx
    toDict :: Type -> Decl
    toDict tp = DAtr z (PWrite z ("d"++ifc++toString' tp)) (ExpWhere (z,e,[])) where
                  e = ECall z (EVar z $ "d"++ifc++ctr)
                        (EVar z $ "d"++ctr++toString' tp)
                  [ctr] = cs  -- TODO: multiple constraints

  -- dIEqBool = Dict.IEq (eq,neq) where eq=<...> daIXxx=... ;
  dict = DAtr z (PWrite z ("d"++ifc++toString' tp)) d where
          d = bool (ExpWhere (z,d2,[])) (ExpWhere (z,d1,decls')) (null cs)

          d1 = ECall z (ECons z ["Dict",ifc])
                       (fromList $ map (EVar z) $ ifceToDeclIds ifce)

          -- func b/c needs a closure with parametric dictionary
          d2 = EFunc z cz TAny [] $ ExpWhere (z,d1,decls'++[ups'])

          -- {daIXxx} // implementation of IOrd for a where a is IXxx
          ups' = DAtr z (fromList $ map (PWrite z) $ L.sort $ map ("da"++) cs)
                    (ExpWhere (z,EArg z,[]))

  toString' (TData hr) = concat hr
  toString' (TVar _)   = concat cs

  -- eq = <...>
  -- TODO: cs
  decls' = concatMap (expandDecl ifces (Ctrs [ifc])) decls

  ifce = fromJust $ L.find h ifces where
          h :: Ifce -> Bool
          h (Ifce (_,id,_,_)) = (id == ifc)

-------------------------------------------------------------------------------

expandDecl :: [Ifce] -> Ctrs -> Decl -> [Decl]

expandDecl _ cs decl@(DSig z id (Ctrs []) tp) = [DSig z id cs tp]

-- Constant (minimum) will ignore dict (dIBoundeda).
-- Function (eq)      will add    dict (dIEqa)      as an upvalue.
--  minimum :: a ->
--    --let dIBoundeda = ... in       -- not required since it will be ignored
--      <...>
--  eq = func :: ((a,a) -> Bool) ->
--    let dIEqa = ... in
--      <...>   -- include {IEqa}
--  f = func :: ((a,a) -> Bool) ->
--    let dIEqa = ... in
--      <...>   -- include {IEqa}

expandDecl _ (Ctrs (_:_)) decl@(DAtr z1 pat1 (ExpWhere (z2,econst2,[]))) | isConst econst2 = [decl'] where
  isConst (EUnit  _)      = True
  isConst (ECons  _ _)    = True
  isConst (ETuple _ es)   = all isConst es
  isConst (ECall  _ f ps) = isConst f && isConst ps
  isConst _               = False

  decl' = DAtr z1 pat1 $ ExpWhere (z2, func, []) where
            func = EFunc z2 cz TAny [] $ ExpWhere (z2, econst2, [])

expandDecl _ (Ctrs []) decl@(DAtr _ _ (ExpWhere (_,EFunc _ (Ctrs []) _ [] _,_))) = [decl]

expandDecl ifces ctrs (DAtr z1 pat1
                        (ExpWhere (z2,
                          EFunc z3 cs3 tp3 [] (ExpWhere (z5,e5,[])),
                          ds2))) =
  [DAtr z1 pat1
    (ExpWhere (z2,
               EFunc z3 ctrs' tp3 ups3' (ExpWhere (z5,e5,ds5')),
               ds2))]
  where
    ctrs' = Ctrs $ ifcesSups ifces (getCtrs cs) where
              cs = case (ctrs,cs3) of
                    (_, Ctrs []) -> ctrs
                    (Ctrs [], _) -> cs3

    -- {daIXxx} // implementation of IOrd for a where a is IXxx
    ups3' = [] --map (\id -> (id,EUnit pz)) $ L.sort $ map ("da"++) imp_ids

    --  <...>               -- original
    --  (f1,...,g1) = d1
    --  (fN,...,gN) = dN
    --  ... = args          -- AUTO
    --  ((d1,...,dN), args) = ...
    ds5' = fsDicts5 ++ [
      DAtr z1 (PArg z1)                             (ExpWhere (z1,EVar z1 "args",[])),
      DAtr z1 (PTuple z1 [dicts5,PWrite z1 "args"]) (ExpWhere (z1,EArg z1,[]))
     ]

    -- [Dict.IEq (eq,neq) = daIEq]
    fsDicts5 :: [Decl]
    fsDicts5 = map f (dicts ctrs') where
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
    dicts5 = fromList $ map (PWrite z1) $ L.sort $ map (\(var,ifc,_) -> 'd':var++ifc) (dicts ctrs')
                                                  -- [daIEq,daIShow,dbIEq,...]

    -- [ (a,IEq,[eq,neq]), (a,IOrd,[...]), (b,...,...), ...]
    dicts :: Ctrs -> [(ID_Var,ID_Ifce,[ID_Var])]
    dicts (Ctrs cs) = f ("a",cs) where
      -- (a,[IEq,IShow]) -> [(a,IEq,[eq,neq]), (a,IOrd,[lt,gt,lte,gte]]
      f :: (ID_Var,[ID_Ifce]) -> [(ID_Var,ID_Ifce,[ID_Var])]
      f (var,ifcs) = map h $ map g ifcs where
                      h :: (ID_Ifce,[ID_Var]) -> (ID_Var,ID_Ifce,[ID_Var])
                      h (ifc,ids) = (var,ifc,ids)
{-
      f :: (ID_Var,[ID_Ifce]) -> [(ID_Var,ID_Ifce,[ID_Var])]
      f (var,ifcs) = map h $ map g ifcs where
                      h :: (ID_Ifce,[ID_Var]) -> (ID_Var,ID_Ifce,[ID_Var])
                      h (ifc,ids) = (var,ifc,ids)
-}

      -- IEq -> (IEq, [eq,neq])
      g :: ID_Ifce -> (ID_Ifce,[ID_Var])
      g ifc = (ifc, ifceToDeclIds $ ifceFind ifces ifc)

expandDecl _ _ decl = [decl]
