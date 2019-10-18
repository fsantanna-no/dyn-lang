module Dyn.Parser where

import Control.Monad          (void, when)
import Data.Char              (isLower, isUpper)

import Text.Parsec.Prim       (many, try, (<|>), (<?>), unexpected)
import Text.Parsec.String     (Parser)
import Text.Parsec.Char       (string, anyChar, newline, oneOf, satisfy, digit, letter)
import Text.Parsec.Combinator (manyTill, eof)

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


