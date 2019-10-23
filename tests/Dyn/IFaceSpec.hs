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
    it "IEq a where a is IXx" $
      run ([r|
main = v where  -- neq (eq(T,T), F)
  v = neq_ ((eq_,neq_), eq_ ((eq_,neq_),Bool.True,Bool.True), Bool.False)

  ixx_bool = f_bool_ where
    f_bool_ = func ->
      eq_ ((eq_,neq_),x,y) where
        ((eq_,neq_),x,y)
;
|] ++ bool ++ ieq)
        `shouldBe` "Bool.True"
-}

-------------------------------------------------------------------------------

-- implementation IOrd for Bool
iord_bool = [r|
  -- dict
  iord_bool = (lt_bool_,lte_bool_,gt_bool_,gte_bool_)
  lt_bool_ = func ->
    case (x,y) of
      (Bool.False, Bool.False) -> Bool.False
      (Bool.False, Bool.True)  -> Bool.True
      (Bool.True,  Bool.False) -> Bool.False
      (Bool.True,  Bool.True)  -> Bool.False
    ; where
      (_,_,x,y) = ...
    ;
  ;
  lte_bool_ = lte_
  gt_bool_  = gt_
  gte_bool_ = gte_
|]

-- instance IEq (Bool)
ieq_bool = [r|
  -- Dict receives eq/neq methods.
  --  - implements eq, uses default neq
  --  - methods receive extra dict
  -- overrides default eq
  ieq_bool  = (eq_bool_,neq_bool_)
  eq_bool_  = func ->  -- (ieq_bool,Bool,Bool) -> Bool
    or (and (x,y), (and (not x, not y))) where
      (_,x,y) = ...
    ;
  ;
  neq_bool_ = neq_
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
  lte_ = func ->  -- (ieq_*,iord_*,a,a) -> Bool
    or ( lt_ ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y),
         eq_ ((eq_,neq_),x,y) ) where
      ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y) = ...
    ;
  ;
  gt_ = func ->  -- (ieq_*,iord_*,a,a) -> Bool
    not (lte_ ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y)) where
      ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y) = ...
    ;
  ;
  gte_ = func ->  -- (ieq_*,iord_*,a,a) -> Bool
    or ( gt_ ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y),
         eq_ ((eq_,neq_),x,y) ) where
      ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y) = ...
    ;
  ;
|]

-- interface IEq(eq,neq)
ieq = [r|
  -- Methods are renamed to include "dict" param:
  --  - eq_  has a default implentation
  --  - neq_ has a default implentation
  ieq = (eq_,neq_)
  eq_ = func ->  -- (ieq_*,a,a) -> Bool
    case (x,y) of
      (~y,~x) -> Bool.True
      _       -> Bool.False
    ; where
      (_,x,y) = ...
    ;
  ;
  neq_ = func ->  -- (ieq_*,a,a) -> Bool
    not (eq_ ((eq_,neq_),x,y)) where
      ((eq_,neq_),x,y) = ...
    ;
  ;
|]
