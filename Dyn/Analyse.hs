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
apply prog@(Prog globs) =
  Prog $
    map globFromDecl $
    Poly.apply globs $ --traceShowSS $ -- [Decl] w/ polys resolved
    Type.apply globs $                 -- [Decl] with types applied/inferred
    map globToDecl   $
    progToGlobs      $
    Ifce.apply       $
    prog where
      progToGlobs (Prog globs) = globs
