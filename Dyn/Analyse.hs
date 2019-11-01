module Dyn.Analyse where

import Dyn.AST
import qualified Dyn.Ifce as Ifce
import qualified Dyn.Poly as Poly

all :: Prog -> Prog
all (Prog globs) =
  Prog $
    map globFromDecl        $
    Poly.poly   ifces []    $ --traceShowSS $
    Ifce.inline ifces impls $ -- [Decl] w/o Ifce/Impl/Gens
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
