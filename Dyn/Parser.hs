module Dyn.Parser where

import Control.Monad          (void, when)
import Data.Char              (isLower, isUpper)

import Text.Parsec.Prim       (many, try, (<|>), (<?>), unexpected, getPosition)
import Text.Parsec.Pos        (SourcePos, sourceLine, sourceColumn)
import Text.Parsec.String     (Parser)
import Text.Parsec.Char       (string, anyChar, newline, oneOf, satisfy, digit, letter)
import Text.Parsec.Combinator (manyTill, eof)

import Dyn.AST                (Expr(..),Ann(..),az)

toPos :: SourcePos -> (Int,Int)
toPos pos = (sourceLine pos, sourceColumn pos)

-------------------------------------------------------------------------------

keywords = [
    "else",
    "error",
    "if",
    "matches",
    "then",
    "where"
  ]

s :: Parser ()
s = void $ many $ (void $ oneOf " ;\n\t") <|> tk_comm

tk_comm :: Parser ()
tk_comm = void $ ((try $ string "--") >> (manyTill anyChar (void newline<|>eof)) <?> "")

tk_var :: Parser String     -- x, x_0       // Xx
tk_var = do
    fst <- satisfy isLower
    rst <- many $ (digit <|> letter <|> oneOf "_'?!" <?> "identifier")
    when (elem (fst:rst) keywords) $ unexpected $ "`" ++ (fst:rst) ++ "`"
    s
    return (fst:rst)

-------------------------------------------------------------------------------

expr_var :: Parser Expr
expr_var = do
  pos <- toPos <$> getPosition
  str <- tk_var
  return $ EVar az{pos=pos} str

expr = expr_var

