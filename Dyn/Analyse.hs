module Dyn.Analyse (apply) where

import Dyn.AST
import qualified Dyn.Parser as P
import qualified Dyn.Eval   as E
import qualified Dyn.Type   as Type
import qualified Dyn.Decl   as Decl
import qualified Dyn.Order  as Order
import qualified Dyn.Ifce   as Ifce
import qualified Dyn.Poly   as Poly

apply :: Prog -> Prog
apply globs =
  Order.apply $
  Decl.apply  $
  Poly.apply  $ --traceShowSS $ -- [Decl] w/ polys resolved
  Type.apply  $                 -- [Decl] with types applied/inferred
  Ifce.apply  $
  globs
