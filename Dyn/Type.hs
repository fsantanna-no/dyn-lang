module Dyn.Type where

import Dyn.AST
import Dyn.Classes

-------------------------------------------------------------------------------

apply :: [Ifce] -> [Decl] -> [Decl]
apply x y = mapDecls (fDz,fE,fPz) x [] y where

  -- apply TType expressions
  -- TType (1+1)  --> TType Nat
  fE :: [Ifce] -> [Decl] -> Expr -> ([Decl],Expr)
  fE _ dsigs (ECall z (ECons z1 ["TType"]) e2) = ([], ETType z $ toTType dsigs e2)
  fE _ _ e = ([], e)

  -- infer TAny decls (only if no DSig found, or DSig is TAny)
  -- x :: ? = 10       --> x :: Nat = 10

{-
  fD :: [Ifce] -> [Decl] -> Decl -> [Decl]

  fD _ dsigs d@(DSig _ _ (_,TAny,_)) = []   -- removes itself (prevents double decl)
  fD _ dsigs d@(DSig _ _ _)          = [d]

  fD _ dsigs (DAtr z (PWrite z1 id1) whe) = [d'] where
    if toTType
    d' = DAtr z1 pat1 $ ExpWhere (z2,e2',ds2)
    (e2',dsE2') = fE (pattToType dsigs pat1) ifces dsigs e2

    pattToType :: [Decl] -> Patt -> Type
    pattToType dsigs (PWrite _ id) = dsigFind dsigs id
    --pattToType _ x = error $ pattToString True x
-}
