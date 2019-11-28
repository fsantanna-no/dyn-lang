module Dyn.Parser where

import Debug.Trace
import Control.Monad          (void, when, guard)
import Data.Maybe             (isNothing,isJust,fromJust)
import Data.Bool              (bool)
import Data.Char              (isLower, isUpper)
import Data.List              (find)

import qualified Text.Parsec as P (parse,parserZero)
import Text.Parsec.Prim       (many, try, (<|>), (<?>), unexpected, getPosition)
import Text.Parsec.Pos        (SourcePos, sourceLine, sourceColumn)
import Text.Parsec.String     (Parser)
import Text.Parsec.Char       (string, anyChar, newline, oneOf, satisfy, digit, letter, char)
import Text.Parsec.Combinator (manyTill, eof, optional, many1, notFollowedBy, option, optionMaybe)

import Dyn.AST
import Dyn.Classes

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
    "data",
    "let",
    "for",
    "implementation",
    "in",
    "interface",
    "recursive",
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
            return $ PArg pos
  lany   = do
            pos  <- toPos <$> getPosition
            void <- tk_sym "_"
            return $ PAny pos
  lwrite = do
            pos  <- toPos <$> getPosition
            void <- bool (tk_sym "=") (tk_sym "") only_write
            var  <- tk_var
            return $ PWrite pos var
  lread  = do
            pos  <- toPos <$> getPosition
            void <- bool (tk_sym "~") P.parserZero only_write
            exp  <- expr
            return $ PRead pos exp
  lcons  = do
            pos  <- toPos <$> getPosition
            cons <- tk_hier
            loc  <- optionMaybe $ try $ pat only_write
            return $ case loc of
                      Nothing -> PCons pos cons
                      Just l  -> PCall pos (PCons pos cons) l
  lunit  = do
            pos  <- toPos <$> getPosition
            void <- tk_sym "("
            void <- tk_sym ")"
            return $ PUnit pos
  lparens = parens $ pat only_write
  ltuple = do
            pos  <- toPos <$> getPosition
            locs <- parens $ list (tk_sym ",") $ pat only_write
            return (PTuple pos $ locs)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

expr :: Parser Expr
expr = try expr_call <|> expr_one

expr_call :: Parser Expr
expr_call = do
  pos <- toPos <$> getPosition
  e1  <- expr_one
  e2  <- expr_one
  --guard $ (fst $ getPos e1) == (fst $ getPos e2)  -- must be at the same line
  return $ ECall pos e1 e2

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
  return $ EError pos "<user>"

expr_arg :: Parser Expr
expr_arg = do
  pos  <- toPos <$> getPosition
  void <- tk_sym "..."
  return $ EArg pos

expr_var :: Parser Expr
expr_var = do
  pos <- toPos <$> getPosition
  str <- tk_var
  return $ EVar pos str

expr_unit :: Parser Expr
expr_unit = do
  pos  <- toPos <$> getPosition
  void <- tk_sym "("
  void <- tk_sym ")"
  return $ EUnit pos

expr_cons :: Parser Expr
expr_cons = do
  pos  <- toPos <$> getPosition
  cons <- tk_hier
  return $ ECons pos cons

expr_tuple :: Parser Expr
expr_tuple = do
  pos  <- toPos <$> getPosition
  exps <- parens $ list (tk_sym ",") expr
  return $ ETuple pos exps

expr_func :: Parser Expr
expr_func = do
  pos  <- toPos <$> getPosition
  void <- tk_key "func"
  (tp,cs) <- option (TAny,Ctrs [])
                    (tk_sym "::" *> type_ctrs)
  ups  <- option [] $
            parensWith (tk_sym "[", tk_sym "]") $
              list (tk_sym ",") tk_var    -- {x}, {x,y}
  void <- tk_sym "{"
  body <- where_let
  void <- tk_sym "}"
  return $ EFunc pos cs tp (map (\id -> (id,EUnit pos)) ups) body

expr_case :: Parser Expr
expr_case = do
  pos  <- toPos <$> getPosition
  void <- tk_key "case"
  e    <- expr
  void <- tk_key "of"
  cs   <- list (tk_sym ";") $ do
            p    <- pat False
            void <- tk_sym "->"
            w    <- where_let
            return (p,w)
  void <- tk_sym "."
  return $ ECase pos e cs

expr_parens :: Parser Expr
expr_parens = parens expr

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

ctrs :: Parser Ctrs
ctrs = do
  void <- try $ tk_key "where"
  cs   <- (singleton <$> ctr) <|> (parens $ list (tk_sym ",") ctr)
  let [("a",ifcs)] = cs
  return $ Ctrs ifcs

ctr :: Parser (ID_Var,[ID_Ifce])
ctr = do
  var  <- tk_var
  guard (var == "a")
  void <- tk_key "is"
  ifcs <- (singleton <$> tk_ifce) <|> (parens $ list (tk_sym ",") $ tk_ifce)
  return (var,ifcs)

-------------------------------------------------------------------------------

type_ctrs :: Parser (Type,Ctrs)
type_ctrs = do
  tp <- type_
  cs <- option (Ctrs []) ctrs
  return (tp,cs)

type_ :: Parser Type
type_ = do
  tp <- try type_A <|> try type_I      <|> try type_D <|> try type_V <|>
        try type_0 <|> try type_parens <|> try type_N <|> type_F <?> "type"
  return tp

type_A :: Parser Type
type_A = do
  void <- tk_sym "?"
  return TAny

type_0 :: Parser Type
type_0 = do
  void <- tk_sym "("
  void <- tk_sym ")"
  return TUnit

type_D :: Parser Type
type_D = do
  hier <- tk_hier
  ofs  <- option TUnit $ try (tk_key "of" *> type_)
  return $ TData hier $ toList ofs

type_I :: Parser Type
type_I = do
  ifcs <- list (tk_sym ",") tk_ifce
  return $ TIfce ifcs

type_N :: Parser Type
type_N = do
  ttps <- parens $ list (tk_sym ",") $ type_
  return $ TTuple ttps

type_F :: Parser Type
type_F = do
  void <- tk_sym "("
  inp  <- type_
  void <- tk_sym "->"
  out  <- type_
  void <- tk_sym ")"
  return $ TFunc inp out

type_V :: Parser Type
type_V = do
  ref <- option False $ do
          void <- try $ tk_key "ref"
          return True
  var <- tk_var
  return $ TVar var

type_parens :: Parser Type
type_parens = do
  void <- tk_sym "("
  tp   <- type_
  void <- tk_sym ")"
  return tp

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

decl_sig :: Parser [Decl]
decl_sig = do
  pos  <- toPos <$> getPosition
  id   <- tk_var
  void <- tk_sym "::"
  (tp,cs) <- type_ctrs
  atr  <- option [] $ do
            void <- tk_sym "="
            w    <- where_let
            return $ singleton $ DAtr pos (PWrite pos id) w
  void <- tk_sym ";"
  return $ (DSig pos id cs tp) : atr

decl_atr :: Parser [Decl]
decl_atr = do
  pos <- toPos <$> getPosition
  pat <- pat True <?> "declaration"
  whe <- do
          void <- tk_sym "="
          w    <- where_let
          return w
  void <- tk_sym ";"
  return $ singleton $ DAtr pos pat whe

decl :: Parser [Decl]
decl = try decl_sig <|> decl_atr

decls :: Parser [Decl]
decls = concat <$> list (tk_sym "") decl

-------------------------------------------------------------------------------

where_let :: Parser ExpWhere
where_let = try let_ <|> where_

let_ :: Parser ExpWhere
let_ = do
  pos  <- toPos <$> getPosition
  void <- tk_key "let"
  ds   <- decls
  void <- tk_key "in"
  whe  <- where_
  void <- tk_sym "."
  let ExpWhere (_,ds',e') = whe
  return $ ExpWhere (pos, ds'++reverse ds, e')

where_ :: Parser ExpWhere
where_ = do
  pos <- toPos <$> getPosition
  e   <- expr
  ds  <- option [] $ do
          void <- try $ tk_key "where"
          ds   <- decls
          void <- tk_sym "."
          return ds
  return $ ExpWhere (pos, ds, e)

-------------------------------------------------------------------------------

data_ :: Parser Data
data_ = do
  pos  <- toPos <$> getPosition
  void <- try $ tk_key "data"
  hr   <- tk_hier
  ofs  <- option [] $ do
            void <- try $ tk_key "of"
            ofs  <- (singleton <$> tk_var) <|> (parens $ list (tk_sym ",") tk_var)
            return ofs
  tp   <- option TUnit $ do
            void <- try $ tk_key "with"
            tp   <- type_
            return tp
  rec  <- optionMaybe $ do
            void <- try (tk_key "is")
            void <- tk_key "recursive"
            return ()
  guard (length hr == 1 || isNothing rec) -- "is recursive" only for base class
  void <- tk_sym ";"
  return $ Data (pos, isJust rec, hr, ofs, tp)

ifce :: Parser Ifce
ifce = do
  pos  <- toPos <$> getPosition
  void <- tk_key "interface"
  cls  <- tk_ifce
  void <- tk_key "for"
  var  <- tk_var
  guard (var == "a")
  cs   <- option (Ctrs []) ctrs
  void <- tk_key "with"
  ds   <- decls
  void <- tk_sym "."
  return $ Ifce (pos, cls, cs, ds)

impl :: Parser Impl
impl = do
  pos  <- toPos <$> getPosition
  void <- tk_key "implementation"
  void <- tk_key "of"
  cls  <- tk_ifce
  void <- tk_key "for"
  (tp,cs) <- type_ctrs
  void <- tk_key "with"
  ds   <- decls
  void <- tk_sym "."
  return $ Impl (pos, cls, cs, tp, ds)

-------------------------------------------------------------------------------

prog :: Parser Prog
prog = do
  pos   <- toPos <$> getPosition
  spc
  globs <- concat <$> list (tk_sym "") (
            (fmap (GDecl<$>) (try decl))        <|>
            (singleton <$> GData <$> try data_) <|>
            (singleton <$> GIfce <$> try ifce)  <|>
            (singleton <$> GImpl <$> impl) -- <|>
           )
  void <- eof
  return globs

-------------------------------------------------------------------------------

parse :: String -> Either String Prog
parse input = parse' prog input

parse' :: Parser a -> String -> Either String a
parse' rule input =
  case P.parse (rule <* eof) "" input of
    (Right v) -> Right v
    (Left  v) -> Left (show v)

parseToStringF :: (Prog->Prog) -> String -> String
parseToStringF f input =
  case parse input of
    (Left  err)  -> err
    (Right prog) -> progToString $ f prog

parseToString :: String -> String
parseToString input = parseToStringF Prelude.id input
