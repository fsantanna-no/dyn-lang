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
taibnd = Type (az,TVar "a",caibnd)
tbool  = Type (az,TData ["Bool"],cz)

ibnd = Ifce (az,"IBounded",[("a",[])],[DSig az "minimum" tz, DSig az "maximum" tz])

main :: IO ()
main = hspec spec

spec = do

  describe "IBounded" $ do

    it "main = maximum" $
      run ("main = maximum" ++ ibounded_bool ++ bool ++ ibounded)
        `shouldBe` "(Bool.True,Bool.False)"

    it "(maximum,minimum)" $
      run ([r|
main = (maximum, minimum)
|] ++ ibounded_bool ++ bool ++ ibounded)
        `shouldBe` "(Bool.True,Bool.False)"

  describe "IEq" $ do

    it "IEq: default eq" $
      run ([r|  -- neq (eq(T,T), F)
main = neq (dIEq(), (eq (dIEq(),(Bool.True,Bool.True)), Bool.False)) where
  Dict.IEq (eq,neq) = dIEq()
;
|] ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: (eq ((T,F),(F,T)), eq ((T,F),(T,F))" $
      run ([r|
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
      run ([r|
main = v where  -- neq (eq(T,T), F)
  v = neq (dIEq(), (eq (dIEq(),(Bool.True,Bool.True)), Bool.False))
  Dict.IEq (eq,neq) = dIEq()
;
|] ++ ieq_bool ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq/IOrd" $
      run ([r|
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
      run ([r|
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
      run ([r|
main = (f ((dIEqBool(),dIOrdBool()), (Bool.True, Bool.False)),
        f ((dIEqBool(),dIOrdBool()), (Bool.False,Bool.False))) where
  f :: ((a,a) -> Bool)
  f = func :: ((a,a) -> Bool) -> gt ...;
;
|] ++ iord_bool ++ ieq_bool ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.True,Bool.False)"

    it "implementation of IEq for a where a is IAaa" $
      run ([r|
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

  describe "poly" $ do
    it "() vs ()" $
      poly [] [] (Type (az,TUnit,cz)) (Where (az,EUnit az,[]))
        `shouldBe` (Where (az,EUnit az,[]))
    it "minimum: Bool vs a::IBounded" $
      toString (poly [ibnd] [DSig az "minimum" taibnd] (Type (az,TData ["Bool"],cz)) (Where (az,EVar az "minimum",[])))
        `shouldBe` "minimum where\n  (Dict.IBounded (minimum,maximum)) = daIBoundedBool\n;"
    it "Bool vs Bool" $
      (poly [] [DSig az "x" tbool] tbool (Where (az,EVar az "x",[])))
        `shouldBe` Where (az,EVar az "x",[])
    it "minimum: a vs a::IBounded" $
      (poly [ibnd] [DSig az "minimum" taibnd] (Type (az,TVar "a",cz)) (Where (az,EVar az "minimum",[])))
        `shouldBe` Where (az,EVar az "minimum",[])
