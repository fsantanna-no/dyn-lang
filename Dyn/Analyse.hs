module Dyn.Analyse where

import Dyn.AST
import Dyn.Parser (parse)
import qualified Dyn.Type as Type
import qualified Dyn.Ifce as Ifce
import qualified Dyn.Poly as Poly

-------------------------------------------------------------------------------

parseToString :: Bool -> String -> String
parseToString shouldAnalyse input =
  case parse input of
    (Left  err)  -> err
    (Right prog) -> toString $ (bool id all shouldAnalyse) prog

evalProg :: Bool -> Prog -> Expr
evalProg shouldAnalyse prog =
  evalExpWhere [] $ ExpWhere (pz, EVar pz "main", map globToDecl glbs') where
    Prog glbs' = (bool id Ana.all shouldAnalyse) prog

-------------------------------------------------------------------------------

all :: Prog -> Prog
all (Prog globs) =
  Prog $
    map globFromDecl        $
    Poly.poly   ifces       $ --traceShowSS $ -- [Decl] w/ polys resolved
    Type.apply  ifces       $                 -- [Decl] with types applied/inferred
    Ifce.inline ifces impls $                 -- [Decl] w/o Ifce/Impl/Gens
    globs
  where
    ifces :: [Ifce]
    ifces = globsToIfces globs
    impls :: [Impl]
    impls = globsToImpls globs

    globsToIfces :: [Glob] -> [Ifce]
    globsToIfces globs = map g $ filter f globs where
                          f (GIfce ifc) = True
                          f _           = False
                          g (GIfce ifc) = ifc

    globsToImpls :: [Glob] -> [Impl]
    globsToImpls globs = map g $ filter f globs where
                          f (GImpl ifc) = True
                          f _           = False
                          g (GImpl ifc) = ifc
