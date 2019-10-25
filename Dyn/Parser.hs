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
import Dyn.Ifce

toPos :: SourcePos -> (Int,Int)
toPos pos = (sourceLine pos, sourceColumn pos)

singleton x = [x]

-------------------------------------------------------------------------------
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

parens :: Parser a -> Parser a
parens p = parensWith (tk_sym "(", tk_sym ")") p

parensWith :: (Parser b,Parser b) -> Parser a -> Parser a
parensWith (open,close) p = do
  void <- open
  ret  <- p
  void <- close
  return ret

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

keywords = [
    "case",
    "error",
    "func",
    "is",
    "of",
    "where",

    -- Xtensions
    "for",
    "implementation",
    "interface",
    "with"
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

tk_key :: String -> Parser ()
tk_key k = do
    key  <- string k
    void <- notFollowedBy (letter <|> char '_' <|> digit)
    guard $ elem key keywords
    spc
    return ()

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

tk_ifce :: Parser ID_Ifce
tk_ifce = do
    fst <- char 'I'
    snd <- satisfy isUpper
    rst <- many $ (digit <|> letter <|> char '_' <?> "interface identifier")
    --guard $ not $ null $ filter (\c -> isLower c) rst
    when (all isUpper rst) $ unexpected "uppercase identifier"
    spc
    return (fst:snd:rst)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- (x, (y,_))
pat :: Bool -> Parser Patt
pat only_write =
  larg        <|>
  lany        <|>
  lwrite      <|>
  lread       <|>
  lcons       <|>
  try lunit   <|>
  try lparens <|>   -- (1-item)
  ltuple      <?> "pattern" where

  larg   = do
            pos  <- toPos <$> getPosition
            void <- tk_sym "..."
            guard only_write
            return $ PArg az{pos=pos}
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
  lparens = parens $ pat only_write
  ltuple = do
            pos  <- toPos <$> getPosition
            locs <- parens $ list (tk_sym ",") $ pat only_write
            return (PTuple az{pos=pos} $ locs)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

expr :: Parser Expr
expr = try expr_call <|> expr_one

expr_call :: Parser Expr
expr_call = do
  pos <- toPos <$> getPosition
  e1  <- expr_one
  e2  <- expr_one
  guard $ (fst $ A.pos $ getAnn e1) == (fst $ A.pos $ getAnn e2)  -- must be at the same line
  return $ ECall az {pos=pos} e1 e2

expr_one :: Parser Expr
expr_one =
  try expr_var    <|>   -- ID_Var
  expr_error      <|>   -- error
  expr_func       <|>   -- func
  expr_case       <|>   -- case

  try expr_unit   <|>   -- ()
  try expr_parens <|>   -- (1-item)
  expr_tuple      <|>   -- (...)

  expr_cons       <|>   -- ID_Data
  expr_arg              -- ...
                  <?> "expression"

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
  exps <- parens $ list (tk_sym ",") expr
  return $ ETuple az{pos=pos} exps

expr_func :: Parser Expr
expr_func = do
  pos  <- toPos <$> getPosition
  void <- tk_key "func"
  tp   <- option tz (tk_sym "::" *> type_)
  ups  <- option [] $
            parensWith (tk_sym "{", tk_sym "}") $
              list (tk_sym ",") tk_var    -- {x}, {x,y}
  void <- tk_sym "->"
  body <- where_
  void <- string ";"
  void <- optional $ try $ tk_key "func"
  spc
  return $ EFunc az{pos=pos} tp (map (\id -> (id,EUnit az{pos=pos})) ups) body

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
  void <- string ";"
  void <- optional $ try $ tk_key "case"
  spc
  return $ ECase az{pos=pos} e cs

expr_parens :: Parser Expr
expr_parens = parens expr

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

type_ :: Parser Type
type_ = do
  pos <- toPos <$> getPosition
  ttp <- ttype
  cs  <- option [] $ try ctrs
  return $ Type (az{pos=pos}, ttp, cs)

ctrs :: Parser TCtrs
ctrs = do
  void <- try $ tk_key "where"
  cs   <- (singleton <$> ctr) <|> (parens $ list (tk_sym ",") $ ctr)
  return cs

ctr :: Parser (ID_Var,[ID_Ifce])
ctr = do
  var  <- tk_var
  void <- tk_key "is"
  ifcs <- (singleton <$> tk_ifce) <|> (parens $ list (tk_sym ",") $ tk_ifce)
  return (var,ifcs)

-------------------------------------------------------------------------------

ttype :: Parser TType
ttype = do
  ttp <- try ttype_D      <|> try ttype_V <|> try ttype_0 <|>
         try ttype_parens <|> try ttype_N <|> ttype_F <?> "type"
  return ttp

ttype_0 :: Parser TType
ttype_0 = do
  void <- tk_sym "("
  void <- tk_sym ")"
  return TUnit

ttype_D :: Parser TType
ttype_D = do
  hier <- tk_hier
  return $ TData hier {-(f ofs)-}

ttype_N :: Parser TType
ttype_N = do
  ttps <- parens $ list (tk_sym ",") $ ttype
  return $ TTuple ttps

ttype_F :: Parser TType
ttype_F = do
  void <- tk_sym "("
  inp  <- ttype
  void <- tk_sym "->"
  out  <- ttype
  void <- tk_sym ")"
  return $ TFunc inp out

ttype_V :: Parser TType
ttype_V = do
  ref <- option False $ do
          void <- try $ tk_key "ref"
          return True
  var <- tk_var
  return $ TVar var

ttype_parens :: Parser TType
ttype_parens = do
  void <- tk_sym "("
  tp   <- ttype
  void <- tk_sym ")"
  return tp

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

decl :: Parser Decl
decl = do
  pos <- toPos <$> getPosition
  p   <- pat True <?> "declaration"
  tp  <- optionMaybe $ do
          void <- try $ tk_sym "::"
          tp   <- type_
          return tp
  w   <- optionMaybe $ do
          void <- tk_sym "="
          w    <- where_
          return w
  guard $ isJust tp || isJust w
  return $ Decl (az{pos=pos}, p, tp, w)

dcls :: Parser [Decl]
dcls = list (tk_sym "") decl

-------------------------------------------------------------------------------

where_ :: Parser Where
where_ = do
  pos <- toPos <$> getPosition
  e   <- expr
  ds  <- option [] $ do
          void <- tk_key "where"
          ds   <- dcls
          void <- string ";"
          void <- optional $ try $ tk_key "where"
          spc
          return ds
  return $ Where (az{pos=pos}, e, ds)

-------------------------------------------------------------------------------

ifce :: Parser Ifce
ifce = do
  pos  <- toPos <$> getPosition
  void <- tk_key "interface"
  cls  <- tk_ifce
  void <- tk_key "for"
  var  <- tk_var
  void <- tk_key "with"
  ds   <- dcls
  void <- string ";"
  void <- optional $ try $ tk_key "interface"
  spc
  return $ Ifce (az{pos=pos}, (cls,var), ds)

impl :: Parser Impl
impl = do
  pos  <- toPos <$> getPosition
  void <- tk_key "implementation"
  void <- tk_key "of"
  cls  <- tk_ifce
  void <- tk_key "for"
  hr   <- tk_hier
  void <- tk_key "with"
  ds   <- dcls
  void <- string ";"
  void <- optional $ try $ tk_key "implementation"
  spc
  return $ Impl (az{pos=pos}, (cls,hr), ds)

-------------------------------------------------------------------------------

-- top-level global declarations
data GList = GDecl Decl | GIfce Ifce | GImpl Impl

prog :: Parser Prog
prog = do
  pos  <- toPos <$> getPosition
  spc
  pl   <- list (tk_sym "") (
            (GDecl <$> try decl) <|>
            (GIfce <$> try ifce) <|>
            (GImpl <$> impl) -- <|>
          )
  void <- eof
  return $
    let
      toDecl (GDecl dcl) = [dcl]
      toDecl (GIfce ifc) = ifceToDecls ifc
      toDecl (GImpl imp) = implToDecls (plToIfcs pl) imp
     in
      Where (az{pos=pos}, EVar az{pos=pos} "main", concatMap toDecl pl)

plToIfcs :: [GList] -> [Ifce]
plToIfcs pl = map g $ filter f pl where
                f (GIfce ifc) = True
                f _           = False
                g (GIfce ifc) = ifc

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
