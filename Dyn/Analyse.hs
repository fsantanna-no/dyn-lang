module Dyn.Analyse where

import Dyn.AST
import qualified Dyn.Ifce as Ifce
import qualified Dyn.Poly as Poly

all :: Prog -> Prog
all (Prog globs) =
  Prog $
    map globFromDecl      $
    Poly.poly ifces []    $ --traceShowSS $
    Ifce.inline    ifces  $
    globs
  where
    ifces :: [Ifce]
    ifces = globsToIfcs globs

    globsToIfcs :: [Glob] -> [Ifce]
    globsToIfcs globs = map g $ filter f globs where
                          f (GIfce ifc) = True
                          f _           = False
                          g (GIfce ifc) = ifc
