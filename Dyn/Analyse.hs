module Dyn.Analyse where

import Dyn.AST
import qualified Dyn.Parser as P
import qualified Dyn.Eval   as E
import qualified Dyn.Type   as Type
import qualified Dyn.Ifce   as Ifce
import qualified Dyn.Poly   as Poly

-------------------------------------------------------------------------------

parseToString :: String -> String
parseToString input = P.parseToStringF apply input

evalString :: String -> String
evalString input = E.evalStringF apply input

-------------------------------------------------------------------------------

apply :: Prog -> Prog
apply prog =
  Prog $
    map globFromDecl $
    Poly.apply ifces $ --traceShowSS $ -- [Decl] w/ polys resolved
    Type.apply ifces $                 -- [Decl] with types applied/inferred
    map globToDecl   $
    globs
  where
    (ifces,Prog globs) = Ifce.apply prog
