{-# LANGUAGE QuasiQuotes #-}

module Dyn.IFace where

-- TODO: Parser.keywords

import Debug.Trace
import Text.RawString.QQ

import Dyn.AST

-------------------------------------------------------------------------------

ifaceToDcls :: IFace -> [Dcl]
ifaceToDcls (IFace (z, (cls,_), dcls)) = dict : dcls where
  -- dIEq = Dict.IEq (eq,neq)
  dict = Dcl (z, PWrite z ("d"++cls), Nothing, Just $ Where (z,e,[]))
  ids = map (\(Dcl (_,PWrite _ id,_,_)) -> id) dcls
  e   = ECall z (ECons z ["Dict",cls]) (listToExpr $ map (EVar z) ids)

implToDcls :: [IFace] -> Impl -> [Dcl]
implToDcls ifcs (Impl (z, (ifc,hr), dcls)) = [dict] where
  -- dIEqBool = Dict.IEq (eq,neq) where eq=...
  dict = Dcl (z, PWrite z ("d"++ifc++concat hr), Nothing, Just $ Where (z,e,[]))
  e    = ECall z (ECons z ["Dict",ifc]) (listToExpr $ map (EVar z) ids)
  ids  = map getId $ getDcls $ head $ filter sameId ifcs where
          sameId  (IFace (_, (id,_), _))    = (id == ifc)
          getDcls (IFace (_, (_,_), dcls))  = dcls
          getId   (Dcl (_,PWrite _ id,_,_)) = id

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
