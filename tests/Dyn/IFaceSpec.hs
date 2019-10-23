{-# LANGUAGE QuasiQuotes #-}

module Dyn.IFaceSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Parser
import Dyn.Eval
import Dyn.Prelude

main :: IO ()
main = hspec spec

spec = do

  describe "manual" $ do

    it "IEq: default eq" $
      run ([r|  -- neq (eq(T,T), F)
main = neq ((eq,neq), eq ((eq,neq),Bool.True,Bool.True), Bool.False) where
  (eq,neq) = ieq
;
|] ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: (eq ((T,F),(F,T)), eq ((T,F),(T,F))" $
      run ([r|
main = (x,y) where
  x = eq ((eq,neq), (Bool.True,Bool.False), (Bool.False,Bool.True))
  y = eq ((eq,neq), (Bool.True,Bool.False), (Bool.True,Bool.False))
  (eq,neq) = ieq
;
|] ++ bool ++ ieq)
        `shouldBe` "(Bool.False,Bool.True)"

    it "IEq: overrides eq (ieq_bool)" $
      run ([r|
main = v where  -- neq (eq(T,T), F)
  v = neq ((eq,neq), eq (ieq,Bool.True,Bool.True), Bool.False)
  (eq,neq) = ieq_bool
;
|] ++ ieq_bool ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq/IOrd" $
      run ([r|
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

    it "implementation of IEq for a where a is IXxx" $
      run ([r|
main = eq ((eq,neq),Xxx,Xxx) where
  (eq,neq) = ieq_ixxx ixxx_xxx
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

{-
    it "f = func :: ((a -> Int) where a is IEq) {a,b} -> eq (x,x)" $
          (run True $
            unlines [
              "interface IEq for a with"          ,
              " var eq  : ((a,a) -> Int)"         ,
              " func neq (x,y) : ((a,a) -> Int) do return 1 - (x eq y) end",
              "end"                               ,
              "implementation of IEq for Int with" ,
              " func eq (x,y) : ((Int,Int) -> Int) do",
              "   if y matches x then return 1 else return 0 end"                  ,
              " end"                              ,
              "end"                               ,
              "func f x : (a -> Int) where a is IEq do",
              "   return x eq x",   -- eq_a
              "end",
              "return f 1"  -- eq_a=eq_Int
             ])
          `shouldBe` Right (EData ["Int","1"] EUnit)
-}
