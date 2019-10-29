{-# LANGUAGE QuasiQuotes #-}

module Dyn.PolyIfceSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Parser
import Dyn.Eval
import Dyn.Prelude
import Dyn.Ifce

caieq  = [("a",["IEq"])]
caibnd = [("a",["IBounded"])]
taibnd = Type (pz,TVar "a",caibnd)
tbool  = Type (pz,TData ["Bool"],cz)

ibnd = Ifce (pz,"IBounded",[("a",[])],[DSig pz "minimum" tz, DSig pz "maximum" tz])

main :: IO ()
main = hspec spec

spec = do

  describe "IBounded" $ do

    it "main = x where x::Bool = maximum;" $
      evalString True ("main = x where x::Bool = maximum;" ++ ibounded_bool ++ bool ++ ibounded)
        `shouldBe` "Bool.True"

    it "(maximum,minimum)" $
      evalString True ([r|
main = (x,y) where
  x :: Bool = maximum
  y :: Bool = minimum
;
|] ++ ibounded_bool ++ bool ++ ibounded)
        `shouldBe` "(Bool.True,Bool.False)"

  describe "IEq" $ do

    it "IEq: default eq" $
      evalString True ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = neq (Bool.True,Bool.False)
;
|] ++ ieq_bool ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "XXX: IEq: default eq" $
      parseToString True ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = neq (eq (Bool.True,Bool.True), Bool.False)
;
|] ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: (eq ((T,F),(F,T)), eq ((T,F),(T,F))" $
      evalString True ([r|
main = (x,y) where
  x = eq (dIEq(), ((Bool.True,Bool.False), (Bool.False,Bool.True )))
  y = eq (dIEq(), ((Bool.True,Bool.False), (Bool.True, Bool.False)))
  Dict.IEq (eq,neq) = dIEq()
;

implementation of IEq for Bool with
  eq = func :: ((Bool,Bool) -> Bool) ->
    or (and (x,y), (and (not x, not y))) where
      (x,y) = ...
    ;
  ;
;
|] ++ bool ++ ieq)
        `shouldBe` "(Bool.False,Bool.True)"

    it "IEq: overrides eq (dieq_bool)" $
      evalString True ([r|
main = v where  -- neq (eq(T,T), F)
  v = neq (dIEq(), (eq (dIEq(),(Bool.True,Bool.True)), Bool.False))
  Dict.IEq (eq,neq) = dIEq()
;
|] ++ ieq_bool ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq/IOrd" $
      evalString True ([r|
main = v where  -- (T<=F, T>=T, F>F, F<T)
  v = ( lte ((dIEqBool(),dIOrdBool()), (Bool.True,  Bool.False)),
        gte ((dIEqBool(),dIOrdBool()), (Bool.True,  Bool.True )),
        gt  ((dIEqBool(),dIOrdBool()), (Bool.False, Bool.False)),
        lt  ((dIEqBool(),dIOrdBool()), (Bool.False, Bool.True )) )
  Dict.IOrd (lt,lte,gt,gte) = dIOrdBool()
;
|] ++ iord_bool ++ ieq_bool ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.False,Bool.True,Bool.False,Bool.True)"

    it "IEq/IOrd/IAaa" $
      evalString True ([r|
main = f ((dIAaa(),dIEq(),dIOrdBool()),(Bool.True,Bool.False)) where
  Dict.IAaa (f) = dIAaa()
;

interface IAaa for a where a is IOrd with
  f :: ((a,a) -> Bool)
  f = func :: ((a,a) -> Bool) -> lt ((daIEq,daIOrd),(x,y)) where (x,y)=...;;  
;
|] ++ iord_bool ++ bool ++ iord ++ ieq)
        `shouldBe` "Bool.False"

    it "f a where a is IOrd" $
      evalString True ([r|
main = (f ((dIEqBool(),dIOrdBool()), (Bool.True, Bool.False)),
        f ((dIEqBool(),dIOrdBool()), (Bool.False,Bool.False))) where
  f :: ((a,a) -> Bool)
  f = func :: ((a,a) -> Bool) -> gt ...;
;
|] ++ iord_bool ++ ieq_bool ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.True,Bool.False)"

    it "implementation of IEq for a where a is IAaa" $
      evalString True ([r|
main = (lt ((dIEq(),dIOrdIAaa (dIAaaXxx())), (Xxx.True,Xxx.False)), gt ((dIEq(),dIOrdIAaa (dIAaaXxx())), (Xxx.True,Xxx.False))) where
  Dict.IOrd (lt,lte,gt,gte) = dIOrdIAaa (dIAaaXxx())
;

implementation of IOrd for a where a is IAaa with
  lt :: ((a,a) -> Bool)
  lt = func :: ((a,a) -> Bool) ->
    lt ((dIEq(),dIOrdBool()), (f (daIAaa,x), f (daIAaa,y))) where
      Dict.IOrd (lt,lte,gt,gte) = dIOrdBool()
      (x,y) = ...
    ;
  ;
;

interface IAaa for a with
  f :: (a -> Bool)
;

implementation of IAaa for Xxx with
  f :: (Xxx -> Bool)
  f = func :: (Xxx -> Bool) ->
    case ... of
      Xxx.True  -> Bool.True
      Xxx.False -> Bool.False
    ;
  ;
;
|] ++ iord_bool ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.False,Bool.True)"

{-
  describe "poly" $ do
    it "() vs ()" $
      poly [] [] (Type (pz,TUnit,cz)) []
        `shouldBe` []
    it "() vs ()" $
      poly [] [] (Type (pz,TUnit,cz)) [DAtr pz (PWrite pz "main") (Where (pz,EUnit pz,[]))]
        `shouldBe` [DAtr pz (PWrite pz "main") (Where (pz,EUnit pz,[]))]
    it "minimum: Bool vs a::IBounded" $
      toString (poly [ibnd] [DSig pz "minimum" taibnd] (Type (pz,TData ["Bool"],cz))
                  [DAtr pz (PWrite pz "main")(Where (pz,EVar pz "minimum",[])))
        `shouldBe` "minimum where\n  (Dict.IBounded (minimum,maximum)) = (dIBoundedBool ())\n;"
    it "Bool vs Bool" $
      (poly [] [DSig pz "x" tbool] tbool (Where (pz,EVar pz "x",[])))
        `shouldBe` Where (pz,EVar pz "x",[])
    it "minimum: a vs a::IBounded" $
      (poly [ibnd] [DSig pz "minimum" taibnd] (Type (pz,TVar "a",cz)) (Where (pz,EVar pz "minimum",[])))
        `shouldBe` Where (pz,EVar pz "minimum",[])
-}
