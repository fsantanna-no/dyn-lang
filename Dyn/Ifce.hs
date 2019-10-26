{-# LANGUAGE QuasiQuotes #-}

module Dyn.Ifce where

-- TODO: Parser.keywords

import Debug.Trace
import Data.Maybe         (fromJust)
import Data.List as L     (find)
import Text.RawString.QQ

import Dyn.AST

-------------------------------------------------------------------------------

-- IEq -> [eq,neq]
ifceToIds :: Ifce -> [ID_Var]
ifceToIds (Ifce (_,_,dcls)) = concatMap f dcls where
                                f (DAtr _ (PWrite _ id) _) = [id]
                                f _ = []

ifceToDecls :: [Ifce] -> Ifce -> [Decl]
ifceToDecls ifces me@(Ifce (z, (ifc,_), dcls)) = dict : dcls' where
  -- dIEq = Dict.IEq (eq,neq)
  dict = DAtr z (PWrite z ("d"++ifc)) (Where (z,e,[])) where
    ids = ifceToIds me
    e   = ECall z (ECons z ["Dict",ifc]) (fromList $ map (EVar z) ids)

  --  eq = func ->
  --    ret where
  --      <...>
  --      -- AUTO
  --      ... = (p1,...pN)
  --      (f1,...,g1) = d1
  --      (fN,...,gN) = dN
  --      (d1,...,dN, p1,...,pN) = ...
  dcls' = map f dcls where
            f (DAtr z1 e1
                (Where (z2,
                        EFunc z3 tp3@(Type (_,TFunc inp3 _,cs3)) ups3 (Where (z4,e4,ds4)),
                        ds2))) = --traceShow (map (declToString 0) dcls) $

              DAtr z1 e1
                (Where (z2,
                        EFunc z3 tp3 ups3 (Where (z4,e4,ds4')),
                        ds2))
              where
                ds4' = ds4 ++ fsDicts ++ [
                          -- (fN,...,gN) = dN
                          -- ... = (p1,...pN)
                          DAtr z1 (PArg z1) (Where (z1,eps,[])),
                          -- (d1,...,dN, p1,...,pN) = ...
                          DAtr z1 pall      (Where (z1,EArg z1,[]))
                         ]

                -- [ (a,IEq,[eq,neq]), (a,IOrd,[...]), (b,...,...), ...]
                dicts :: [(ID_Var,ID_Ifce,[ID_Var])]
                dicts = concatMap f cs3 where
                  -- (a,[IEq,IShow]) -> [(a,IEq,[eq,neq]), (a,IOrd,[lt,gt,lte,gte]]
                  f :: (ID_Var,[ID_Ifce]) -> [(ID_Var,ID_Ifce,[ID_Var])]
                  f (var,ifcs) = map h $ map g ifcs where
                                  h :: (ID_Ifce,[ID_Var]) -> (ID_Var,ID_Ifce,[ID_Var])
                                  h (ifc,ids) = (var,ifc,ids)

                  -- IEq -> (IEq, [eq,neq])
                  g :: ID_Ifce -> (ID_Ifce,[ID_Var])
                  g ifc = (ifc, ids) where
                            ids = ifceToIds $ fromJust $ L.find h ifces where
                                    h :: Ifce -> Bool
                                    h (Ifce (_,(id,_),_)) = (id == ifc)

                -- [Dict.IEq (eq,neq) = daIEq]
                fsDicts :: [Decl]
                fsDicts = map f dicts where
                  f :: (ID_Var,ID_Ifce,[ID_Var]) -> Decl
                  f (var,ifc,ids) = DAtr z1 pat (Where (z1,exp,[])) where
                    -- Dict.IEq (eq,neq)
                    pat :: Patt
                    pat = PCall z1 (PCons z1 ["Dict",ifc]) (fromList $ map (PWrite z1) ids)
                    -- daIEq
                    exp :: Expr
                    exp = EVar z1 $ 'd':var++ifc

                -- (p1,...,pN)
                eps = fromList $ map (EVar z1) $ ps

                -- (d1,...,dN, p1,...,pN)
                pall = fromList $ dicts' ++ ps' where
                  ps'    = map (PWrite z1) $ ps
                  dicts' = map (PWrite z1) $ map (\(var,ifc,_) -> 'd':var++ifc) dicts
                                              -- [daIEq,daIShow,dbIEq,...]

                -- [p1,...,pN]
                ps :: [ID_Var]
                ps = map ('p':) $ map show $ take (length $ toList inp3) incs where
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
      ;
    ;
    gt = func:: ((a,a) -> Bool) where a is (IEq,IOrd) ->
      not (lte (daIEq,daIOrd,x,y)) where
        (x,y) = ...
      ;
    ;
    gte = func:: ((a,a) -> Bool) where a is (IEq,IOrd) ->
      or ( gt (daIEq,daIOrd,x,y),
           eq (daIEq,x,y) ) where
        (x,y) = ...
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
