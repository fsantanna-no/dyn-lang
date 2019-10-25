{-# LANGUAGE QuasiQuotes #-}

module Dyn.Ifce where

-- TODO: Parser.keywords

import Debug.Trace
import Text.RawString.QQ

import Dyn.AST

-------------------------------------------------------------------------------

ifceToDecls :: Ifce -> [Decl]
ifceToDecls (Ifce (z, (ifc_id,ifc_var), dcls)) = dict : dcls' where
  -- dIEq = Dict.IEq (eq,neq)
  dict = DAtr z (PWrite z ("d"++ifc_id)) (Where (z,e,[])) where
    ids = map (\(DAtr _ (PWrite _ id) _) -> id) $ filter isDAtr dcls
    e   = ECall z (ECons z ["Dict",ifc_id]) (fromList $ map (EVar z) ids)

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
    f (DAtr z1 pat1
        (Where (z2,
                EFunc z3 (Type (z4,TFunc inp4 out4,cs4)) ups3 (Where (z5,e5,ds5)),
                ds2))) =

      DAtr z1 pat1
        (Where (z2,
                EFunc z3 (Type (z4,TFunc inp4 out4,cs4')) ups3 (Where (z5,e5,ds5')),
                ds2))
      where
        -- interface IEq for a
        --    eq :: ((a,a) -> Bool)
        --    eq :: ((a,a) -> Bool) where a is IEq
        cs4' = (ifc_var, [ifc_id]) :  cs4

        ds5' = ds5 ++ [
                  -- ... = (p1,...pN)
                  DAtr z1 (PArg z1) (Where (z1,eps,[])),
                  -- (d1,...,dN, p1,...,pN) = ...
                  DAtr z1 pall      (Where (z1,EArg z1,[]))
                 ]

        -- [daIEq,daIShow,dbIEq,...]
        dicts = concatMap f cs4' where
                  f (var,ifcs) = map (('d':var)++) ifcs

        -- (p1,...,pN)
        eps = fromList $ map (EVar z1) $ ps

        -- (d1,...,dN, p1,...,pN)
        pall = fromList $ dicts' ++ ps' where
          dicts' = map (PWrite z1) $ dicts
          ps'    = map (PWrite z1) $ ps

        -- [p1,...,pN]
        ps :: [ID_Var]
        ps = map ('p':) $ map show $ take (length $ toList inp4) incs where
              incs :: [Int]
              incs = 1 : map (+1) incs

implToDecls :: [Ifce] -> Impl -> [Decl]
implToDecls ifcs (Impl (z, (ifc,hr), dcls)) = [dict] where
  -- dIEqBool = Dict.IEq (eq,neq) where eq=...
  dict = DAtr z (PWrite z ("d"++ifc++concat hr)) (Where (z,e,dcls))
  e    = ECall z (ECons z ["Dict",ifc]) (fromList $ map (EVar z) ids)
  ids  = map getId $ getDecls $ head $ filter sameId ifcs where
          sameId   (Ifce (_, (id,_), _))    = (id == ifc)
          getDecls (Ifce (_, (_,_), dcls))  = dcls
          getId    (DAtr _ (PWrite _ id) _) = id
  --dcls' = traceShow (map (declToString 0) dcls) $ map f dcls where
          --f (Decl (z,e,tp,Just wh)) = traceShow (whereToString 0 wh) $ Decl (z,e,tp,Just wh)

-------------------------------------------------------------------------------

ieq = [r|
  interface IEq for a with
    eq = func :: ((a,a) -> Bool) ->
      case (x,y) of
        (~y,_) -> Bool.True
        _      -> Bool.False
      ; where
        (x,y) = ...
      ;
    ;
    neq = func :: ((a,a) -> Bool) ->
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
