{-# LANGUAGE QuasiQuotes #-}

module Dyn.Sugar.IFace where

-- TODO: Parser.keywords

import Debug.Trace
import Data.List as L
import Data.Char              (isLower, isUpper)
import Control.Monad          (void, when, guard)

import Text.RawString.QQ
import Text.Parsec.Prim       (many, try, (<|>), (<?>), unexpected, getPosition)
import Text.Parsec.String     (Parser)
import Text.Parsec.Char       (satisfy, digit, letter, char)
import Text.Parsec.Combinator (optional, option)

import Dyn.AST
import Dyn.Parser

{-
dcl_impl :: Parser [Dcl]
dcl_impl = do
  pos  <- toPos <$> getPosition
  void <- tk_key "implementation"
  void <- tk_key "of"
  cls  <- tk_class
  void <- tk_key "for"
  hr   <- tk_hier
  void <- tk_key "with"
  ds   <- dcls sugar
  void <- tk_sym ";"
  void <- optional $ tk_key "implementation"
  return $
    let
      z    = az{pos=pos}
      dict = Dcl (z, PWrite z ("d"++cls), Nothing, Just $ Where (z,e,[]))
      ids  = map (\(Dcl (_,PWrite _ id,_,_)) -> id) ds
      e    = ECall z (ECons z ["Dict",cls]) (listToExpr $ map (EVar z) ids)
     in
      Dcl (z, PWrite z ("d"++cls++concat hr), Nothing, Just $ Where (z,e,ds))
-}

-------------------------------------------------------------------------------

sugar = (dcl_iface)

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
