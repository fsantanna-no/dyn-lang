{-# LANGUAGE QuasiQuotes #-}

module Dyn.Ifce where

-- TODO: Parser.keywords

import Debug.Trace
import Text.RawString.QQ

import Dyn.AST

-------------------------------------------------------------------------------

ifceToDecls :: Ifce -> [Decl]
ifceToDecls (Ifce (z, (cls,_), dcls)) = dict : dcls' where
  -- dIEq = Dict.IEq (eq,neq)
  dict = Decl (z, PWrite z ("d"++cls), Nothing, Just $ Where (z,e,[])) where
    ids = map (\(Decl (_,PWrite _ id,_,_)) -> id) dcls
    e   = ECall z (ECons z ["Dict",cls]) (listToExpr $ map (EVar z) ids)

  -- TODO:
  --    - (da,...,dx)  -- respect generic constrained vars
  --    - from types, discover pN, g, dN
  --
  --  eq = func ->
  --    ret where
  --      <...>
  --      -- AUTO
  --      ... = (p1,...pN)
  --      (f1,...,g1) = d1
  --      (fN,...,gN) = dN
  --      (d1,...,dN, p1,...,pN) = ...
  dcls' = map f dcls where
            f (Decl (z1,e1,tp1,
                Just (Where (z2,
                             EFunc z3 tp3 ups3 (Where (z4,e4,ds4)),
                             ds2)))) =

              Decl (z1,e1,tp1,
                Just (Where (z2,
                             EFunc z3 tp3 ups3 (Where (z4,e4,ds4')),
                             ds2)))
              where
                ds4' = ds4 ++ [
                          Decl (z1, PArg z1, Nothing, Just $ Where (z1,eps,[])),
                          Decl (z1, pall, Nothing, Just $ Where (z1,EArg z1,[]))
                         ]

                -- (p1,...,pN)
                eps = listToExpr $ take 2 $ map (EVar z1) $ map ('p':) incs'

                -- (d1,...,dN, p1,...,pN)
                pall = listToPatt $ ds ++ ps where
                  ds = take 1 $ map (PWrite z1) $ map ('d':) incs'
                  ps = take 2 $ map (PWrite z1) $ map ('p':) incs'

                -- [1,2,...]
                incs :: [Int]
                incs = 1 : map (+1) incs
                incs' = map show incs

implToDecls :: [Ifce] -> Impl -> [Decl]
implToDecls ifcs (Impl (z, (ifc,hr), dcls)) = [dict] where
  -- dIEqBool = Dict.IEq (eq,neq) where eq=...
  dict = Decl (z, PWrite z ("d"++ifc++concat hr), Nothing, Just $ Where (z,e,dcls))
  e    = ECall z (ECons z ["Dict",ifc]) (listToExpr $ map (EVar z) ids)
  ids  = map getId $ getDecls $ head $ filter sameId ifcs where
          sameId   (Ifce (_, (id,_), _))    = (id == ifc)
          getDecls (Ifce (_, (_,_), dcls))  = dcls
          getId    (Decl (_,PWrite _ id,_,_)) = id
  --dcls' = traceShow (map (declToString 0) dcls) $ map f dcls where
          --f (Decl (z,e,tp,Just wh)) = traceShow (whereToString 0 wh) $ Decl (z,e,tp,Just wh)

-------------------------------------------------------------------------------

ieq = [r|
  interface IEq for a with
    eq = func ->  -- (a,a) -> Bool
      case (x,y) of
        (~y,_) -> Bool.True
        _      -> Bool.False
      ; where
        (x,y) = ...
      ;
    ;
    neq = func ->  -- (a,a) -> Bool
      not (eq (d1,x,y)) where
        (x,y) = ...
      ;
    ;
  ;
|]

iord = [r|
  interface IOrd for a with
    lt  = ()
    lte = func ->  -- (ieq_*,iord_*,a,a) -> Bool
      or ( lt (dieq,diord,x,y),
           eq (dieq,x,y) ) where
        (x,y) = ...
        -- AUTO
        ... = (p1,p2)
        Dict.IEq (eq,neq) = dieq
        Dict.IOrd (lt,lte,gt,gte) = diord
        (dieq,diord,p1,p2) = ...
      ;
    ;
    gt = func ->  -- (ieq_*,iord_*,a,a) -> Bool
      not (lte (dieq,diord,x,y)) where
        (x,y) = ...
        -- AUTO
        ... = (p1,p2)
        Dict.IEq (eq,neq) = dieq
        Dict.IOrd (lt,lte,gt,gte) = diord
        (dieq,diord,p1,p2) = ...
        ;
    ;
    gte = func ->  -- (ieq_*,iord_*,a,a) -> Bool
      or ( gt (dieq,diord,x,y),
           eq (dieq,x,y) ) where
        (x,y) = ...
        -- AUTO
        ... = (p1,p2)
        Dict.IEq (eq,neq) = dieq
        Dict.IOrd (lt,lte,gt,gte) = diord
        (dieq,diord,p1,p2) = ...
      ;
    ;
  ;
|]

-------------------------------------------------------------------------------

ieq_bool = [r|
  implementation of IEq for Bool with
    eq = func ->  -- (dIEqBool,Bool,Bool) -> Bool
      or (and (x,y), (and (not x, not y))) where
        (x,y) = ...
        -- AUTO
        ... = (p1,p2)
        (dieq,p1,p2) = ...
      ;
    ;
  ;
|]

iord_bool = [r|
  implementation of IOrd for Bool with
    lt = func ->
      case (x,y) of
        (Bool.False, Bool.False) -> Bool.False
        (Bool.False, Bool.True)  -> Bool.True
        (Bool.True,  Bool.False) -> Bool.False
        (Bool.True,  Bool.True)  -> Bool.False
      ; where
        (x,y) = ...
        -- AUTO
        ... = (p1,p2)
        (dieq,diord,p1,p2) = ...
      ;
    ;
  ;
|]
