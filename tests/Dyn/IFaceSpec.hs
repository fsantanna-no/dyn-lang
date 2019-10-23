{-# LANGUAGE QuasiQuotes #-}

module Dyn.IFaceSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Parser
import Dyn.Eval

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

{-
    it "IEq a where a is IXxx" $
      run ([r|
main = eq ((eq,neq), x1,x2) where
  (eq,neq) = ieq_ixxx
;

ieq_ixxx  = (eq_ixxx_,neq_ixxx_)
eq_ixxx_  = func ->
  eq () f x where
    ((f), x,y) = ...
;
neq_ixxx_ = neq_

ixxx_xxx = f_xxx
f_xxx -> Bool.True ;

|] ++ bool ++ ieq)
        `shouldBe` "Bool.True"
-}

-------------------------------------------------------------------------------

-- implementation IOrd for Bool
iord_bool = [r|
  -- dict
  iord_bool = (lt,lte,gt,gte) where
    lt = func ->
      case (x,y) of
        (Bool.False, Bool.False) -> Bool.False
        (Bool.False, Bool.True)  -> Bool.True
        (Bool.True,  Bool.False) -> Bool.False
        (Bool.True,  Bool.True)  -> Bool.False
      ; where
        (_,_,x,y) = ...
      ;
    ;
  ;
|]

-- instance IEq (Bool)
ieq_bool = [r|
  -- Dict receives eq/neq methods.
  --  - implements eq, uses default neq
  --  - methods receive extra dict
  -- overrides default eq
  ieq_bool = (eq_bool,neq_bool) where
    eq_bool  = func ->  -- (ieq_bool,Bool,Bool) -> Bool
      or (and (x,y), (and (not x, not y))) where
        (_,x,y) = ...
      ;
    ;
    neq_bool = neq
  ;
|]

-- Bool type: not, and, or
bool = [r|
  not = func ->
    case ... of
      Bool.False -> Bool.True
      Bool.True  -> Bool.False
    ;
  ;

  and = func ->
    case ... of
      (Bool.False, _) -> Bool.False
      (_, Bool.False) -> Bool.False
      _               -> Bool.True
    ;
  ;

  or = func ->
    case ... of
      (Bool.True, _)  -> Bool.True
      (_,         =y) -> y
    ;
  ;
|]

-- interface IOrd(lt,lte,dt,gte)
iord = [r|
  -- lt_ = ???
  lte = func ->  -- (ieq_*,iord_*,a,a) -> Bool
    or ( lt ((eq,neq),(lt,lte,gt,gte),x,y),
         eq ((eq,neq),x,y) ) where
      ((eq,neq),(lt,lte,gt,gte),x,y) = ...
    ;
  ;
  gt = func ->  -- (ieq_*,iord_*,a,a) -> Bool
    not (lte ((eq,neq),(lt,lte,gt,gte),x,y)) where
      ((eq,neq),(lt,lte,gt,gte),x,y) = ...
    ;
  ;
  gte = func ->  -- (ieq_*,iord_*,a,a) -> Bool
    or ( gt ((eq,neq),(lt,lte,gt,gte),x,y),
         eq ((eq,neq),x,y) ) where
      ((eq,neq),(lt,lte,gt,gte),x,y) = ...
    ;
  ;
|]

-- interface IEq(eq,neq)
ieq = [r|
  -- Methods are renamed to include "dict" param:
  --  - eq_  has a default implentation
  --  - neq_ has a default implentation
  ieq = (eq,neq)  -- IEq is an interface with all members instantiated, so it support all types
  eq = func ->  -- (ieq_*,a,a) -> Bool
    case (x,y) of
      (~y,~x) -> Bool.True
      _       -> Bool.False
    ; where
      (_,x,y) = ...
    ;
  ;
  neq = func ->  -- (ieq_*,a,a) -> Bool
    not (eq ((eq,neq),x,y)) where
      ((eq,neq),x,y) = ...
    ;
  ;
|]
