module Dyn.Type where

import Debug.Trace

import Dyn.AST
import Dyn.Classes

-------------------------------------------------------------------------------

apply :: [Ifce] -> [Decl] -> [Decl]
apply x y = mapDecls (fD,fE,fPz) x cz [] y where

  -- apply Type expressions
  -- Type (1+1)  --> Type Nat
  fE :: [Ifce] -> Ctrs -> [Decl] -> Expr -> Expr
  fE _ _ dsigs (ECall z (ECons z1 ["Type"]) e2) = EType z $ toType dsigs e2
  fE _ _ _ e = e

  -- infer TAny decls (only if no DSig found, or DSig is TAny)
  -- x :: ? = 10       --> x :: Nat = 10

  fD :: [Ifce] -> Ctrs -> [Decl] -> Decl -> [Decl]

  --fD _ _ dsigs d@(DSig _ _ (_,TAny,_)) = []   -- removes itself (prevents double decl)
  --fD _ _ dsigs d@(DSig _ _ _)          = [d]

  fD _ _ dsigs datr@(DAtr z (PWrite z1 id1) whe) = [dsig,datr] where
    dsig = DSig z id1 cz (toType dsigs whe)

  fD _ _ _ d = [d]
{-
    if toTType
    d' = DAtr z1 pat1 $ ExpWhere (z2,e2',ds2)
    (e2',dsE2') = fE (pattToType dsigs pat1) ifces dsigs e2

    pattToType :: [Decl] -> Patt -> Type
    pattToType dsigs (PWrite _ id) = dsigFind dsigs id
    --pattToType _ x = error $ pattToString True x
-}
