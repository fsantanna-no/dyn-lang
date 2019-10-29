module Dyn.Analyse where

import Dyn.AST
import qualified Dyn.Ifce as Ifce

all :: Prog -> Prog
all (Prog globs) =
  Prog $
    map globFromDecl        $
    Ifce.polyDecls ifces []      $
    concatMap remGIfceGImpl $
    globs
  where
    remGIfceGImpl :: Glob -> [Decl]
    remGIfceGImpl (GDecl dcl) = [dcl]
    remGIfceGImpl (GIfce ifc) = Ifce.ifceToDecls ifces ifc
    remGIfceGImpl (GImpl imp) = Ifce.implToDecls ifces imp
    ifces = globsToIfcs globs

    globsToIfcs :: [Glob] -> [Ifce]
    globsToIfcs globs = map g $ filter f globs where
                          f (GIfce ifc) = True
                          f _           = False
                          g (GIfce ifc) = ifc
