{-# LANGUAGE QuasiQuotes #-}

module Dyn.PolyManual (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Parser
import Dyn.Eval
import Dyn.Prelude

main :: IO ()
main = hspec spec

spec = do

  describe "IEq" $ do

    it "IEq: default eq" $
      run sgz ([r|  -- neq (eq(T,T), F)
main = neq ((eq,neq), eq ((eq,neq),Bool.True,Bool.True), Bool.False) where
  (eq,neq) = ieq
;
|] ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: (eq ((T,F),(F,T)), eq ((T,F),(T,F))" $
      run sgz ([r|
main = (x,y) where
  x = eq ((eq,neq), (Bool.True,Bool.False), (Bool.False,Bool.True))
  y = eq ((eq,neq), (Bool.True,Bool.False), (Bool.True,Bool.False))
  (eq,neq) = ieq
;
|] ++ bool ++ ieq)
        `shouldBe` "(Bool.False,Bool.True)"

    it "IEq: overrides eq (ieq_bool)" $
      run sgz ([r|
main = v where  -- neq (eq(T,T), F)
  v = neq ((eq,neq), eq (ieq,Bool.True,Bool.True), Bool.False)
  (eq,neq) = ieq_bool
;
|] ++ ieq_bool ++ bool ++ ieq)
        `shouldBe` "Bool.True"

  describe "IEq/IOrd" $ do

    it "IEq/IOrd" $
      run sgz ([r|
main = v where  -- (T<=F, T>=T, F>F, F<T)
  v = ( lte ((eq,neq), (lt,lte,gt,gte), Bool.True,  Bool.False),
        gte ((eq,neq), (lt,lte,gt,gte), Bool.True,  Bool.True),
        gt  ((eq,neq), (lt,lte,gt,gte), Bool.False, Bool.False),
        lt  ((eq,neq), (lt,lte,gt,gte), Bool.False, Bool.True) )
  (eq,neq)        = ieq_bool
  (lt,lte,gt,gte) = iord_bool
;
|] ++ iord_bool ++ ieq_bool ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.False,Bool.True,Bool.False,Bool.True)"

  describe "HKT" $ do

    it "implementation of IEq for a where a is IXxx" $
      run sgz ([r|
main = eq ((eq,neq),Xxx,Xxx) where
  (eq,neq) = ieq_ixxx ixxx_xxx      -- higher-kinded types (HKT)?
;

ieq_ixxx = func -> -- ixxx -> ieq
  (eq,neq) where
    eq = func {f} ->  -- :: (ieq_xxx,a,a) -> Bool where a is IXxx
      eq ((eq,neq), f ((f),x), f ((f),y)) where
        (eq,neq) = ieq_bool
        (_,x,y)  = ...
      ;
    ; where
      (f) = ...
    ;
  ;
;

ixxx_xxx = f where
  f = func -> -- :: (ixxx_xxx,X) -> Bool
    case x of
      Xxx -> Bool.True
    ; where
      (_,x) = ...
    ;
  ;
;
|] ++ ieq_bool ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "f = func :: ((a -> Int) where a is IEq) {a,b} -> eq (x,x)" $
      run sgz ([r|
main = f (ieq_nat, one)
f = func ->
  eq ((eq,neq),x,x) where
    ((eq,neq), x) = ...
  ;
;
|] ++ prelude)
          `shouldBe` "Bool.True"
