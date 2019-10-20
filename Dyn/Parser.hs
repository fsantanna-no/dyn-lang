module Dyn.Parser where

import Debug.Trace
import Control.Monad          (void, when, guard)
import Data.Maybe             (isJust)
import Data.Bool              (bool)
import Data.Char              (isLower, isUpper)

import qualified Text.Parsec as P (parse,parserZero)
import Text.Parsec.Prim       (many, try, (<|>), (<?>), unexpected, getPosition)
import Text.Parsec.Pos        (SourcePos, sourceLine, sourceColumn)
import Text.Parsec.String     (Parser)
import Text.Parsec.Char       (string, anyChar, newline, oneOf, satisfy, digit, letter, char)
import Text.Parsec.Combinator (manyTill, eof, optional, many1, notFollowedBy, option, optionMaybe)

import Dyn.AST as A

toPos :: SourcePos -> (Int,Int)
toPos pos = (sourceLine pos, sourceColumn pos)

-------------------------------------------------------------------------------

-- LISTS
-- `,` separator:
--    a,b,c,
-- Optional separator at the end.

list :: Parser a -> Parser b -> Parser [b]
list sep p = do
  v    <- p
  vs   <- many $ try $ sep *> p
  void <- optional $ try $ sep
  return (v:vs)

-------------------------------------------------------------------------------

keywords = [
    "case",
    "else",
    "error",
    "func",
    "if",
    "then",
    "of",
    "where"
  ]

spc :: Parser ()
spc  = void $ many $ (void $ oneOf " \t\n") <|> tk_comm

tk_comm :: Parser ()
tk_comm = void $ ((try $ string "--") >> (manyTill anyChar (void newline<|>eof)) <?> "")

tk_sym :: String -> Parser ()
tk_sym str = do
    void <- string str
    spc
    return ()

tk_key :: String -> Parser String
tk_key k = do
    key  <- string k
    void <- notFollowedBy (letter <|> char '_' <|> digit)
    guard $ elem key keywords
    spc
    return key

tk_var :: Parser String     -- x, x_0       // Xx
tk_var = do
    fst <- satisfy isLower
    rst <- many $ (digit <|> letter <|> oneOf "_'?!" <?> "identifier")
    when (elem (fst:rst) keywords) $ unexpected $ "`" ++ (fst:rst) ++ "`"
    spc
    return (fst:rst)

tk_data :: Parser String    -- Int, Int_0   // I, II, int, _Int
tk_data = do
    fst <- satisfy isUpper
    rst <- many $ (digit <|> letter <|> char '_' <?> "data identifier")
    spc
    return (fst:rst)

tk_hier :: Parser ID_Hier
tk_hier = do
  v <- (:) <$> tk_data <*> many (try $ tk_sym "." *> tk_data)
  return v

-------------------------------------------------------------------------------

-- (x, (y,_))
pat :: Bool -> Parser Patt
pat only_write = lany <|> lwrite <|> lread <|> lcons <|> try lunit <|> ltuple <?> "pattern" where
  lany   = do
            pos  <- toPos <$> getPosition
            void <- tk_sym "_"
            return $ PAny az{pos=pos}
  lwrite = do
            pos  <- toPos <$> getPosition
            void <- bool (tk_sym "=") (tk_sym "") only_write
            var  <- tk_var
            return $ PWrite az{pos=pos} var
  lread  = do
            pos  <- toPos <$> getPosition
            void <- bool (tk_sym "~") P.parserZero only_write
            exp  <- expr
            return $ PRead az{pos=pos} exp
  lcons  = do
            pos  <- toPos <$> getPosition
            cons <- tk_hier
            loc  <- optionMaybe $ try $ pat only_write
            return $ case loc of
                      Nothing -> PCons az{pos=pos} cons
                      Just l  -> PCall az{pos=pos} (PCons az{pos=pos} cons) l
  lunit  = do
            pos  <- toPos <$> getPosition
            void <- tk_sym "("
            void <- tk_sym ")"
            return $ PUnit az{pos=pos}
  ltuple = do
            pos  <- toPos <$> getPosition
            void <- tk_sym "("
            locs <- list (tk_sym ",") $ pat only_write
            void <- tk_sym ")"
            return (PTuple az{pos=pos} $ locs)

-------------------------------------------------------------------------------

expr_error :: Parser Expr
expr_error = do
  pos  <- toPos <$> getPosition
  void <- tk_key "error"
  return $ EError az{pos=pos} "<user>"

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
  void <- tk_sym "("
  exps <- list (tk_sym ",") expr
  void <- tk_sym ")"
  return $ ETuple az{pos=pos} exps

expr_func :: Parser Expr
expr_func = do
  pos  <- toPos <$> getPosition
  void <- tk_key "func"
  void <- tk_sym "("
  void <- tk_sym ")"
  body <- where_
  void <- tk_sym ";"
  void <- optional $ tk_key "func"
  return $ EFunc az{pos=pos} tz body

expr_if :: Parser Expr
expr_if = do
  pos  <- toPos <$> getPosition
  void <- tk_key "if"
  e    <- expr
  void <- tk_sym "~"
  p    <- pat False
  void <- tk_key "then"
  t    <- where_
  void <- tk_key "else"
  f    <- where_
  void <- tk_sym ";"
  void <- optional $ tk_key "if"
  return $ EIf az{pos=pos} e p t f

expr_case :: Parser Expr
expr_case = do
  pos  <- toPos <$> getPosition
  void <- tk_key "case"
  e    <- expr
  void <- tk_key "of"
  cs   <- list (tk_sym "") $ do
            p    <- pat False
            void <- tk_sym "->"
            w    <- where_
            return (p,w)
  void <- tk_sym ";"
  void <- optional $ tk_key "case"
  return $ ECase az{pos=pos} e cs

expr_parens :: Parser Expr
expr_parens = do
  void <- tk_sym "("
  e    <- expr
  void <- tk_sym ")"
  return e

expr_one :: Parser Expr
expr_one =
  try expr_var    <|>   -- ID_Var
  expr_error      <|>   -- error
  expr_func       <|>   -- func
  expr_if         <|>   -- if
  expr_case       <|>   -- case

  try expr_unit   <|>   -- ()
  try expr_parens <|>   -- (1-item)
  expr_tuple      <|>   -- (...)

  expr_cons       <|>   -- ID_Data
  expr_arg              -- ...
                  <?> "expression"

expr_call :: Parser Expr
expr_call = do
  pos <- toPos <$> getPosition
  e1  <- expr_one
  e2  <- expr_one
  guard $ (fst $ A.pos $ getAnn e1) == (fst $ A.pos $ getAnn e2)  -- must be at the same line
  return $ ECall az {pos=pos} e1 e2

expr :: Parser Expr
expr = try expr_call <|> expr_one

-------------------------------------------------------------------------------

type_ :: Parser Type
type_ = do
  void <- tk_sym "("
  void <- tk_sym ")"
  return TUnit

-------------------------------------------------------------------------------

dcl :: Parser Dcl
dcl = do
  pos <- toPos <$> getPosition
  p   <- pat True
  tp  <- optionMaybe $ do
          void <- try $ tk_sym "::"
          tp   <- type_
          return tp
  w   <- optionMaybe $ do
          void <- tk_sym "="
          w    <- where_
          return w
  guard $ isJust tp || isJust w
  return $ Dcl (az{pos=pos}, p, tp, w)

-------------------------------------------------------------------------------

where_ :: Parser Where
where_ = do
  pos  <- toPos <$> getPosition
  e    <- expr
  dcls <- option [] $ do
            void <- tk_key "where"
            dcls <- list (tk_sym "") dcl
            void <- tk_sym ";"
            void <- optional $ tk_key "where"
            return dcls
  return $ Where (az{pos=pos}, e, dcls)

-------------------------------------------------------------------------------

prog :: Parser Prog
prog = do
  pos  <- toPos <$> getPosition
  spc
  dcls <- list (tk_sym "") dcl
  void <- eof
  return $ Where (az{pos=pos}, EVar az{pos=pos} "main", dcls)

-------------------------------------------------------------------------------

parse :: String -> Either String Prog
parse input = parse' prog input

parse' :: Parser a -> String -> Either String a
parse' rule input =
  case P.parse (rule <* eof) "" input of
    (Right v) -> Right v
    (Left  v) -> Left (show v)

parseToString :: String -> String
parseToString input =
  case parse input of
    (Left  v) -> v
    (Right p) -> progToString p
