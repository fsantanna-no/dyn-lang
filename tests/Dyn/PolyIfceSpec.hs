{-# LANGUAGE QuasiQuotes #-}

module Dyn.PolyIfceSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Parser
import Dyn.Eval
import Dyn.Prelude hiding (ieq,iord,ieq_bool,iord_bool)
import Dyn.Ifce

main :: IO ()
main = hspec spec

spec = do

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

    it "IEq/IOrd/IXxx" $
      run ([r|
main = f ((dIEq(),dIOrdBool(),dIXxx()),(Bool.True,Bool.False)) where
  Dict.IXxx (f) = dIXxx()
;

interface IXxx for a where a is IOrd with
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

    it "XXX-1: implementation of IEq for a where a is IXxx" $
      run ([r|
main = (lt ((dIEq(),dIOrdIXxx (dIXxxXxx())), (Xxx.True,Xxx.False)), gt ((dIEq(),dIOrdIXxx (dIXxxXxx())), (Xxx.True,Xxx.False))) where
  Dict.IOrd (lt,lte,gt,gte) = dIOrdIXxx (dIXxxXxx())
;

implementation of IOrd for a where a is IXxx with
  lt :: ((a,a) -> Bool)
  lt = func :: ((a,a) -> Bool) ->
    lt ((dIEq(),dIOrdBool()), (f (daIXxx,x), f (daIXxx,y))) where
      Dict.IOrd (lt,lte,gt,gte) = dIOrdBool()
      (x,y) = ...
    ;
  ;
;

interface IXxx for a with
  f :: (a -> Bool)
;

implementation of IXxx for Xxx with
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
