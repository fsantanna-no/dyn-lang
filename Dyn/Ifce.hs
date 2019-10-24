{-# LANGUAGE QuasiQuotes #-}

module Dyn.Ifce where

-- TODO: Parser.keywords

import Debug.Trace
import Text.RawString.QQ

import Dyn.AST

-------------------------------------------------------------------------------

ifceToDecls :: Ifce -> [Decl]
ifceToDecls (Ifce (z, (cls,_), dcls)) = dict : dcls where
  -- dIEq = Dict.IEq (eq,neq)
  dict = Decl (z, PWrite z ("d"++cls), Nothing, Just $ Where (z,e,[]))
  ids = map (\(Decl (_,PWrite _ id,_,_)) -> id) dcls
  e   = ECall z (ECons z ["Dict",cls]) (listToExpr $ map (EVar z) ids)

implToDecls :: [Ifce] -> Impl -> [Decl]
implToDecls ifcs (Impl (z, (ifc,hr), dcls)) = [dict] where
  -- dIEqBool = Dict.IEq (eq,neq) where eq=...
  dict = Decl (z, PWrite z ("d"++ifc++concat hr), Nothing, Just $ Where (z,e,dcls))
  e    = ECall z (ECons z ["Dict",ifc]) (listToExpr $ map (EVar z) ids)
  ids  = map getId $ getDecls $ head $ filter sameId ifcs where
          sameId   (Ifce (_, (id,_), _))    = (id == ifc)
          getDecls (Ifce (_, (_,_), dcls))  = dcls
          getId    (Decl (_,PWrite _ id,_,_)) = id

-------------------------------------------------------------------------------

ieq = [r|
  interface IEq for a with
    eq = func ->  -- (a,a) -> Bool
      case (x,y) of
        (~y,_) -> Bool.True
        _      -> Bool.False
      ; where
        (_,x,y) = ...
      ;
    ;
    neq = func ->  -- (a,a) -> Bool
      not (eq (Dict.IEq (eq,neq),x,y)) where
        (Dict.IEq (eq,neq),x,y) = ...
      ;
    ;
  ;
|]

iord = [r|
  interface IOrd for a with
    lt  = ()
    lte = func ->  -- (ieq_*,iord_*,a,a) -> Bool
      or ( lt (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y),
           eq (Dict.IEq (eq,neq),x,y) ) where
        (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y) = ...
      ;
    ;
    gt = func ->  -- (ieq_*,iord_*,a,a) -> Bool
      not (lte (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y)) where
        (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y) = ...
      ;
    ;
    gte = func ->  -- (ieq_*,iord_*,a,a) -> Bool
      or ( gt (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y),
           eq (Dict.IEq (eq,neq),x,y) ) where
        (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y) = ...
      ;
    ;
  ;
|]

-------------------------------------------------------------------------------

ieq_bool = [r|
  implementation of IEq for Bool with
    eq = func ->  -- (dIEqBool,Bool,Bool) -> Bool
      or (and (x,y), (and (not x, not y))) where
        (_,x,y) = ...
      ;
    ;
  ;
|]

iord_bool = [r|
  implementation of IOrd for Bool with
    lt = func ->
      case (x,y) of
        (Bool.False, Bool.False) -> Bool.False
        (Bool.False, Bool.True)  -> Bool.True
        (Bool.True,  Bool.False) -> Bool.False
        (Bool.True,  Bool.True)  -> Bool.False
      ; where
        (_,_,x,y) = ...
      ;
    ;
  ;
|]
