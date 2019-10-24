{-# LANGUAGE QuasiQuotes #-}

module Dyn.IFace where

-- TODO: Parser.keywords

import Debug.Trace
import Text.RawString.QQ

import Dyn.AST

-------------------------------------------------------------------------------

ifaceToDcls :: IFace -> [Dcl]
ifaceToDcls (IFace (z, (cls,_), dcls)) = dcl : dcls where
  dcl = Dcl (z, PWrite z ("d"++cls), Nothing, Just $ Where (z,e,[]))
  ids = map (\(Dcl (_,PWrite _ id,_,_)) -> id) dcls
  e   = ECall z (ECons z ["Dict",cls]) (listToExpr $ map (EVar z) ids)

implToDcls :: Impl -> [Dcl]
implToDcls impl = []

{-
      dict = Dcl (z, PWrite z ("d"++cls), Nothing, Just $ Where (z,e,[]))
      ids  = map (\(Dcl (_,PWrite _ id,_,_)) -> id) ds
      e    = ECall z (ECons z ["Dict",cls]) (listToExpr $ map (EVar z) ids)
     in
      Dcl (z, PWrite z ("d"++cls++concat hr), Nothing, Just $ Where (z,e,ds))
-}

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
