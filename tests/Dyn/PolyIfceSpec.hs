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

    it "IEq: default eq" $
      evalString True ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = neq (eq (Bool.True,Bool.True), Bool.False)
;
|] ++ ieq_bool ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: overrides eq (dieq_bool)" $
      evalString True ([r|
main = v where  -- neq (eq(T,T), F)
  v = neq ((eq (Bool.True,Bool.True), Bool.False))
;
|] ++ ieq_bool ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq/IOrd" $
      evalString True ([r|
main = v where  -- (T<=F, T>=T, F>F, F<T)
  v = ( lte (Bool.True,  Bool.False),
        gte (Bool.True,  Bool.True ),
        gt  (Bool.False, Bool.False),
        lt  (Bool.False, Bool.True ) )
;
|] ++ iord_bool ++ ieq_bool ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.False,Bool.True,Bool.False,Bool.True)"

    it "XXX: IEq/IOrd/IAaa" $
      evalString True ([r|
main = f (Bool.True,Bool.False)

implementation of IAaa for Bool with
  f :: ((Bool,Bool) -> Bool)
;

interface IAaa for a where a is IOrd with
  f :: ((a,a) -> Bool)
  f = func :: ((a,a) -> Bool) -> lt ... ;     -- TODO: dispatch
;
|] ++ iord_bool ++ ieq_bool ++ bool ++ iord ++ ieq)
        `shouldBe` "Bool.False"

    it "ff1/ff2" $
      evalString True ([r|
main = (ff1 (lte, (Bool.True,Bool.False)),
        ff2 (gte, (Bool.True,Bool.True )) ) where
  ff1 :: (((a,a)->Bool), -> Bool)             -- TODO: should work with previous
  ff1 = func -> f (x,y) where (f,x,y)=... ;;

  ff2 :: (((Bool,Bool)->Bool) -> Bool)        -- TODO: needs closures
  ff2 = func -> f (x,y) where (f,x,y)=... ;;
;
|] ++ prelude)
        `shouldBe` "(Bool.False,Bool.True)"

    it "f a where a is IOrd" $
      evalString True ([r|
main = (f (Bool.True, Bool.False),
        f (Bool.False,Bool.False)) where
  f :: ((a,a) -> Bool)
  f = func :: ((a,a) -> Bool) -> gt ...;
;
|] ++ prelude)
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
