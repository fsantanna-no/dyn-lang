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

type ID_Var  = String

tk_class :: Parser String    -- Int, Int_0   /no/ I, II, int, _Int
tk_class = do
    fst <- char 'I'
    snd <- satisfy isUpper
    rst <- many $ (digit <|> letter <|> char '_' <?> "interface identifier")
    --guard $ not $ null $ filter (\c -> isLower c) rst
    when (all isUpper rst) $ unexpected "uppercase identifier"
    spc
    return (fst:snd:rst)

dcl_iface :: Parser [Dcl]
dcl_iface = do
  pos  <- toPos <$> getPosition
  void <- tk_key "interface"
  cls  <- tk_class
  void <- tk_key "for"
  var  <- tk_var
  --ctx  <- option [] $ try pContext
  void <- tk_key "with"
  ds   <- dcls dcl_iface
  void <- tk_sym ";"
  void <- optional $ tk_key "interface"
  return $
    let
      z    = az{pos=pos}
      dict = Dcl (z, PWrite z ("d"++cls), Nothing, Just $ Where (z,e,[]))
      ids  = map (\(Dcl (_,PWrite _ id,_,_)) -> id) ds
      e    = ECall z (ECons z ["Dict",cls]) (listToExpr $ map (EVar z) ids)
     in
      dict : ds

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
