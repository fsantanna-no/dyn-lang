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
  dict = Decl (z, PWrite z ("d"++ifc++concat hr), Nothing, Just $ Where (z,e,[]))
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
