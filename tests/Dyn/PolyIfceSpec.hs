{-# LANGUAGE QuasiQuotes #-}

module Dyn.PolyIfceSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import qualified Dyn.Parser as P
import Dyn.Eval
import Dyn.Prelude hiding (ieq)
import Dyn.Ifce

main :: IO ()
main = hspec spec

spec = do

  describe "IEq" $ do

    it "IEq: default eq" $
      run ([r|  -- neq (eq(T,T), F)
main = neq (dIEq, eq (dIEq,Bool.True,Bool.True), Bool.False) where
  Dict.IEq (eq,neq) = dIEq
;

interface IEq for a with
  eq = func ->  -- (a,a) -> Bool
    case (x,y) of
      (~y,_) -> Bool.True
      _      -> Bool.False
    ; where
      (_,x,y) = ...
    ;
  ;
  neq = func ->  -- (a,a) -> Bool
    not (eq (Dict.IEq (eq,neq),x,y)) where
      (Dict.IEq (eq,neq),x,y) = ...
    ;
  ;
;
|] ++ bool)
        `shouldBe` "Bool.True"

    it "XXX: IEq: (eq ((T,F),(F,T)), eq ((T,F),(T,F))" $
      run ([r|
main = (x,y) where
  x = eq (dieq, (Bool.True,Bool.False), (Bool.False,Bool.True))
  y = eq (dieq, (Bool.True,Bool.False), (Bool.True,Bool.False))
  Dict.IEq (eq,neq) = dieq
;

implementation of IEq for Bool with
  eq = func ->  -- (dieq_bool,Bool,Bool) -> Bool
    or (and (x,y), (and (not x, not y))) where
      (_,x,y) = ...
    ;
  ;
;
|] ++ bool ++ ieq)
        `shouldBe` "(Bool.False,Bool.True)"

{-
    it "IEq: overrides eq (dieq_bool)" $
      run ([r|
main = v where  -- neq (eq(T,T), F)
  v = neq (dieq_bool, eq (dieq_bool,Bool.True,Bool.True), Bool.False)
  Dict.IEq (eq,neq) = dieq_bool
;
|] ++ ieq_bool ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq/IOrd" $
      run ([r|
main = v where  -- (T<=F, T>=T, F>F, F<T)
  v = ( lte (dieq_bool, diord_bool, Bool.True,  Bool.False),
        gte (dieq_bool, diord_bool, Bool.True,  Bool.True),
        gt  (dieq_bool, diord_bool, Bool.False, Bool.False),
        lt  (dieq_bool, diord_bool, Bool.False, Bool.True) )
  Dict.IEq  (eq,neq)        = dieq_bool
  Dict.IOrd (lt,lte,gt,gte) = diord_bool
;
|] ++ iord_bool ++ ieq_bool ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.False,Bool.True,Bool.False,Bool.True)"

    it "implementation of IEq for a where a is IXxx" $
      run ([r|
main = eq (Dict.IEq (eq,neq),Xxx,Xxx) where
  Dict.IEq (eq,neq) = ieq_ixxx ixxx_xxx      -- higher-kinded types (HKT)?
;

ieq_ixxx = func -> -- ixxx -> ieq
  Dict.IEq (eq,neq) where
    eq = func {f} ->  -- :: (ieq_xxx,a,a) -> Bool where a is IXxx
      eq (Dict.IEq (eq,neq), f ((f),x), f ((f),y)) where
        Dict.IEq (eq,neq) = dieq_bool
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
      run ([r|
main = f (ieq_nat, one)
f = func ->
  eq (Dict.IEq (eq,neq),x,x) where
    (Dict.IEq (eq,neq), x) = ...
  ;
;
|] ++ prelude)
          `shouldBe` "Bool.True"
-}
