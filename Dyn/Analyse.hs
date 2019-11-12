module Dyn.Analyse (apply) where

import Dyn.AST
import qualified Dyn.Parser as P
import qualified Dyn.Eval   as E
import qualified Dyn.Type   as Type
import qualified Dyn.Order  as Order
import qualified Dyn.Ifce   as Ifce
import qualified Dyn.Poly   as Poly

apply :: Prog -> Prog -> Prog
apply origs globs =
  --Order.apply origs $
  Poly.apply  origs $ --traceShowSS $ -- [Decl] w/ polys resolved
  Type.apply  origs $                 -- [Decl] with types applied/inferred
  Ifce.apply  origs $
  globs
