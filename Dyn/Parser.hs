module Dyn.Parser where

import Control.Monad          (void, when, guard)
import Data.Bool              (bool)
import Data.Char              (isLower, isUpper)

import Text.Parsec.Prim       (many, try, (<|>), (<?>), unexpected, getPosition)
import Text.Parsec.Pos        (SourcePos, sourceLine, sourceColumn)
import Text.Parsec.String     (Parser)
import Text.Parsec.Char       (string, anyChar, newline, oneOf, satisfy, digit, letter, char)
import Text.Parsec.Combinator (manyTill, eof, optional, many1, notFollowedBy)

import Dyn.AST

toPos :: SourcePos -> (Int,Int)
toPos pos = (sourceLine pos, sourceColumn pos)

list :: Bool -> Parser a -> Parser [a]
list one p = do
    void <- tk_sym "("
    v    <- p
    vs   <- (bool many1 many one) $ try $ tk_sym "," *> p
    void <- optional $ try $ tk_sym ","
    void <- tk_sym ")"
    return (v:vs)

list1 = list True
list2 = list False

-------------------------------------------------------------------------------

keywords = [
    "else",
    "error",
    "func",
    "if",
    "matches",
    "then",
    "where"
  ]

s :: Parser ()
s = void $ many $ (void $ oneOf " ;\n\t") <|> tk_comm

tk_comm :: Parser ()
tk_comm = void $ ((try $ string "--") >> (manyTill anyChar (void newline<|>eof)) <?> "")

tk_sym :: String -> Parser ()
tk_sym str = do
    void <- string str
    s
    return ()

tk_key :: String -> Parser String
tk_key k = do
    key  <- string k
    void <- notFollowedBy (letter <|> char '_' <|> digit)
    guard $ elem key keywords
    s
    return key

tk_var :: Parser String     -- x, x_0       // Xx
tk_var = do
    fst <- satisfy isLower
    rst <- many $ (digit <|> letter <|> oneOf "_'?!" <?> "identifier")
    when (elem (fst:rst) keywords) $ unexpected $ "`" ++ (fst:rst) ++ "`"
    s
    return (fst:rst)

tk_data :: Parser String    -- Int, Int_0   // I, II, int, _Int
tk_data = do
    fst <- satisfy isUpper
    rst <- many $ (digit <|> letter <|> char '_' <?> "data identifier")
    s
    return (fst:rst)

tk_hier :: Parser ID_Hier
tk_hier = do
  v <- (:) <$> tk_data <*> many (try $ tk_sym "." *> tk_data)
  return v

-------------------------------------------------------------------------------

expr_error :: Parser Expr
expr_error = do
  pos  <- toPos <$> getPosition
  void <- tk_key "error"
  return $ EError az{pos=pos}

expr_arg :: Parser Expr
expr_arg = do
  pos  <- toPos <$> getPosition
  void <- tk_sym "..."
  return $ EArg az{pos=pos}

expr_var :: Parser Expr
expr_var = do
  pos <- toPos <$> getPosition
  str <- tk_var
  return $ EVar az{pos=pos} str

expr_unit :: Parser Expr
expr_unit = do
  pos  <- toPos <$> getPosition
  void <- tk_sym "("
  void <- tk_sym ")"
  return $ EUnit az{pos=pos}

expr_cons :: Parser Expr
expr_cons = do
  pos  <- toPos <$> getPosition
  cons <- tk_hier
  return $ ECons az{pos=pos} cons

expr_tuple :: Parser Expr
expr_tuple = do
  pos  <- toPos <$> getPosition
  exps <- try $ list2 expr
  return $ ETuple az{pos=pos} exps

expr_func :: Parser Expr
expr_func = do
  pos  <- toPos <$> getPosition
  void <- tk_key "func"
  void <- tk_sym "("
  void <- tk_sym ")"
  body <- expr
  return $ EFunc az{pos=pos} () body

expr_if :: Parser Expr
expr_if = do
  pos  <- toPos <$> getPosition
  void <- tk_key "if"
  e    <- expr
  void <- tk_key "matches"
  p    <- expr
  void <- tk_sym "then"
  t    <- expr
  void <- tk_sym "else"
  f    <- expr
  return $ EIf az{pos=pos} e p t f

expr_parens :: Parser Expr
expr_parens = do
  void <- tk_sym "("
  e    <- expr
  void <- tk_sym ")"
  return e

expr_one :: Parser Expr
expr_one =
  try expr_error  <|>
  try expr_var    <|>
  try expr_unit   <|>
  expr_cons       <|>
  expr_tuple      <|>
  expr_func       <|>
  expr_arg        <|>
  expr_if         <|>
  expr_parens     <?> "expression"

expr_call :: Parser Expr
expr_call = do
  pos <- toPos <$> getPosition
  e1  <- expr_one
  e2  <- expr_one
  return $ ECall az {pos=pos} e1 e2

expr :: Parser Expr
expr = try expr_call <|> expr_one

-------------------------------------------------------------------------------

dcl :: Parser Dcl
dcl = do
  str  <- tk_var
  void <- tk_sym "::"
  void <- tk_sym "("
  void <- tk_sym ")"
  void <- tk_sym "="
  w    <- where_
  return $ Dcl (str, (), w)

-------------------------------------------------------------------------------

where_ :: Parser Where
where_ = do
  e <- expr
  return $ Where (e, [])
