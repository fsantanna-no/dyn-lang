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

    it "IEqualable: default neq" $
      run ([r|
main = v where  -- neq (eq(T,T), F)
  -- typesystem renames to concrete type methods
  v = neq_bool (eq_bool (Bool.True,Bool.True), Bool.False)
;

|] ++ ieq_bool ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEqualable/IOrderable" $
      run ([r|
main = v where  -- (T<=F, T>=T, F>F, F<T)
  v = ( lte_bool (Bool.True,  Bool.False),
        gte_bool (Bool.True,  Bool.True),
        gt_bool  (Bool.False, Bool.False),
        lt_bool  (Bool.False, Bool.True) )
;
|] ++ iord_bool ++ ieq_bool ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.False,Bool.True,Bool.False,Bool.True)"

-------------------------------------------------------------------------------

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

-- implementation IOrderable for Bool
iord_bool = [r|
  -- wrappers
  lt_bool  = func -> lt_  (ieq_bool,iord_bool,x,y) where
              (x,y)        = ...
              (lt_,_,_,_)  = iord_bool
             ;;
  lte_bool = func -> lte_ (ieq_bool,iord_bool,x,y) where
              (x,y)        = ...
              (_,lte_,_,_) = iord_bool
             ;;
  gt_bool  = func -> gt_  (ieq_bool,iord_bool,x,y) where
              (x,y)        = ...
              (_,_,gt_,_)  = iord_bool
             ;;
  gte_bool = func -> gte_ (ieq_bool,iord_bool,x,y) where
              (x,y)        = ...
              (_,_,_,gte_) = iord_bool
             ;;
  -- dict
  iord_bool = (lt_bool_,lte_,gt_,gte_) where
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
  ;
|]

-- instance IEq (Bool)
ieq_bool = [r|
  -- Wrappers are closures with fixed "ieq_bool" dict
  eq_bool  = func -> eq_ (ieq_bool,x,y) where
              (x,y)   = ...
              (eq_,_) = ieq_bool
             ;;
  neq_bool = func -> neq_ (ieq_bool,x,y) where
              (x,y)    = ...
              (_,neq_) = ieq_bool
             ;;

  -- Dict receives eq/neq methods.
  --  - implements eq, uses default neq
  --  - methods receive extra dict
  ieq_bool = (eq_bool_,neq_) where
    eq_bool_ = func ->
      or (and (x,y), (and (not x, not y))) where
        (_,x,y) = ...
      ;
    ;
  ;
|]

-- interface IOrderable
iord = [r|
  -- lt_ = ???
  lte_ = func ->
    or ( lt_ ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y),
         eq_ ((eq_,neq_),x,y) ) where
      ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y) = ...
    ;
  ;
  gt_ = func ->
    not (lte_ ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y)) where
      ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y) = ...
    ;
  ;
  gte_ = func ->
    or ( gt_ ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y),
         eq_ ((eq_,neq_),x,y) ) where
      ((eq_,neq_),(lt_,lte_,gt_,gte_),x,y) = ...
    ;
  ;
|]

-- interface IEq(eq,neq)
ieq = [r|
  -- Methods are renamed to include "dict" param:
  --  - eq_ is not implemented
  --  - neq_ has a default implentation
  -- eq_ = ???
  neq_ = func ->
    not (eq_ ((eq_,neq_),x,y)) where
      ((eq_,neq_),x,y) = ...
    ;
  ;
|]
