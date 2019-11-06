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

  fD :: [Ifce] -> Ctrs -> [Decl] -> Decl -> [Decl]

  fD _ _ dsigs d@(DSig _ _ _ TAny) = []   -- removes itself (prevents double decl)
  fD _ _ dsigs d@(DSig _ _ _ _)    = [d]

  -- x :: ? = 10       --> x :: Nat = 10
  fD _ _ dsigs datr@(DAtr z pat1@(PWrite z1 id1) whe2@(ExpWhere (z2,ds2,e2))) = dsig'++[datr'] where
    (dsig',datr') = case (toType dsigs pat1, toType dsigs whe2) of
      (TAny, TAny) -> ([],                 datr)
      (TAny, tp)   -> ([DSig z id1 cz tp], datr)
      (tp,   TAny) -> ([],                 DAtr z pat1 (ExpWhere (z2,ds2,e2')))
      otherwise    -> ([],                 datr)  -- TODO: check types
      where
        e2' = e2  -- TODO
        e2' = infer ifces dsigs' tp e2 where
                dsigs' = dsigs ++ filter isDSig ds2

  fD _ _ _ d = [d]

infer :: [Ifce] -> [Decl] -> Type -> Expr -> Expr

-- pat::Bool = id(maximum)
infer ifces dsigs xtp e@(EVar z id) = e' where

  (cs,_) = dsigsFind dsigs id
  cs'    = Ifce.ifcesSups ifces (getCtrs cs) where

  e' = case (cs', xtp) of
    ([], _)        -> e                          -- var is not poly, nothing to do
    (_, TData xhr) -> xxx z cs' (concat xhr) id  -- xtp is concrete
    (_, TVar _   ) -> xxx z cs' "a"          id  -- xtp is not concrete yet
    otherwise      -> e

-- pat1::B = id2(neq) e2::(B,B)
infer ifces dsigs xtp e@(ECall z1 e2@(EVar z2 id2) e3) = ECall z1 e2' e3' where

  e3' = infer ifces dsigs xtp e3

  (cs2,tp2) = dsigsFind dsigs id2
  cs2'      = Ifce.ifcesSups ifces (getCtrs cs2) where

  e2' = case (cs2', tp2) of
    ([], _)               -> e2      -- var is not poly, nothing to do
    (_,  TFunc inp2 out2) ->
      case xhr inp2 out2 of          --   ... and xtp is concrete -> resolve!
        Left ()           -> e2
        Right (Just xhr)  -> xxx z2 cs2' (concat xhr) id2
        Right Nothing     -> xxx z2 cs2' "a"          id2 -- xtp is not concrete yet
    otherwise             -> e2      -- var is not function, ignore

  xhr inp2 out2 = --traceShow (id2, toString e, toString e3, toType dsigs e3) $
    case tpMatch (TTuple [inp2             , out2])
                 (TTuple [toType dsigs e3' , xtp ]) of
      [("a", TData xhr)] -> Right $ Just xhr
      [("a", TVar  "a")] -> Right $ Nothing
      otherwise          -> Left ()
        where
          -- eq :: (a,a) -> Bool
          [tvar2] = toVars tp2   -- [a]
          -- a is Bool

infer ifces dsigs xtp (ECall z1 e2 e3) =
  ECall z1 e2' e3' where
    e2' = infer ifces dsigs TAny e2
    e3' = infer ifces dsigs TAny e3

infer ifces dsigs _ (ETuple z es) = ETuple z $ map (infer ifces dsigs TAny) es

infer ifces dsigs _ (EFunc  z1 cs1 tp1 ups1 (ExpWhere (z2,ds2,e2))) =
  EFunc z1 cs1 tp1 ups1 (ExpWhere (z2,ds2,e2')) where
    e2' = infer ifces dsigs' TAny e2 where
            dsigs' = dsigs ++ filter isDSig ds2

infer ifces dsigs xtp (ECase z e l) = ECase z e' l' where
  e' = infer ifces dsigs TAny e
  l' = map f l where
        -- TODO: pat
        f (pat, ExpWhere (z,ds,e)) = (pat, ExpWhere (z,ds,infer ifces dsigs' xtp e)) where
                                      dsigs' = dsigs ++ filter isDSig ds

infer _ _ _ e@(EArg  _)   = e
infer _ _ _ e@(EUnit _)   = e
infer _ _ _ e@(EVar  _ _) = e
infer _ _ _ e@(ECons _ _) = e
infer _ _ _ e@(EType _ _) = e

infer _ _ _ e = error $ show e


