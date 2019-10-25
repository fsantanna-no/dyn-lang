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
  --    - (fN,...,gN) = dN
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
                             EFunc z3 tp3@(Type (_,TFunc inp3 _,cs3)) ups3 (Where (z4,e4,ds4)),
                             ds2)))) =

              Decl (z1,e1,tp1,
                Just (Where (z2,
                             EFunc z3 tp3 ups3 (Where (z4,e4,ds4')),
                             ds2)))
              where
                ds4' = ds4 ++ [
                          -- ... = (p1,...pN)
                          Decl (z1, PArg z1, Nothing, Just $ Where (z1,eps,[])),
                          -- (d1,...,dN, p1,...,pN) = ...
                          Decl (z1, pall, Nothing, Just $ Where (z1,EArg z1,[]))
                         ]

                -- [daIEq,daIShow,dbIEq,...]
                dicts = concatMap f cs3 where
                          f (var,ifcs) = map (('d':var)++) ifcs

                -- (p1,...,pN)
                eps = listToExpr $ map (EVar z1) $ ps

                -- (d1,...,dN, p1,...,pN)
                pall = listToPatt $ dicts' ++ ps' where
                  dicts' = map (PWrite z1) $ dicts
                  ps'    = map (PWrite z1) $ ps

                -- [p1,...,pN]
                ps :: [ID_Var]
                ps = map ('p':) $ map show $ take (length $ ttypeToList inp3) incs where
                      incs :: [Int]
                      incs = 1 : map (+1) incs

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
    eq = func :: ((a,a) -> Bool) where a is IEq ->
      case (x,y) of
        (~y,_) -> Bool.True
        _      -> Bool.False
      ; where
        (x,y) = ...
      ;
    ;
    neq = func :: ((a,a) -> Bool) where a is IEq ->
      not (eq (daIEq,x,y)) where
        (x,y) = ...
      ;
    ;
  ;
|]

iord = [r|
  interface IOrd for a with
    lt  = ()
    lte = func :: ((a,a) -> Bool) where a is (IEq,IOrd) ->
      or ( lt (daIEq,daIOrd,x,y),
           eq (daIEq,x,y) ) where
        (x,y) = ...
        -- AUTO
        Dict.IEq (eq,neq) = daIEq
        Dict.IOrd (lt,lte,gt,gte) = daIOrd
      ;
    ;
    gt = func:: ((a,a) -> Bool) where a is (IEq,IOrd) ->
      not (lte (daIEq,daIOrd,x,y)) where
        (x,y) = ...
        -- AUTO
        Dict.IEq (eq,neq) = daIEq
        Dict.IOrd (lt,lte,gt,gte) = daIOrd
        ;
    ;
    gte = func:: ((a,a) -> Bool) where a is (IEq,IOrd) ->
      or ( gt (daIEq,daIOrd,x,y),
           eq (daIEq,x,y) ) where
        (x,y) = ...
        -- AUTO
        Dict.IEq (eq,neq) = daIEq
        Dict.IOrd (lt,lte,gt,gte) = daIOrd
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
        (daIEq,p1,p2) = ...
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
        (daIEq,daIOrd,p1,p2) = ...
      ;
    ;
  ;
|]
