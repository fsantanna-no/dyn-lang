{-# LANGUAGE QuasiQuotes #-}

module Dyn.PolyManualSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Parser
import Dyn.Eval
import Dyn.Prelude (nat,bool)

main :: IO ()
main = hspec spec

spec = do

  describe "IEq" $ do

    it "IEq: default eq" $
      run ([r|  -- neq (eq(T,T), F)
main = neq (dieq, eq (dieq,Bool.True,Bool.True), Bool.False) where
  Dict.IEq (eq,neq) = dieq
;
|] ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: (eq ((T,F),(F,T)), eq ((T,F),(T,F))" $
      run ([r|
main = (x,y) where
  x = eq (dieq, (Bool.True,Bool.False), (Bool.False,Bool.True))
  y = eq (dieq, (Bool.True,Bool.False), (Bool.True,Bool.False))
  Dict.IEq (eq,neq) = dieq
;
|] ++ bool ++ ieq)
        `shouldBe` "(Bool.False,Bool.True)"

    it "IEq: overrides eq (dieq_bool)" $
      run ([r|
main = v where  -- neq (eq(T,T), F)
  v = neq (dieq, eq (dieq,Bool.True,Bool.True), Bool.False)
  Dict.IEq (eq,neq) = dieq
;
|] ++ ieq_bool ++ bool ++ ieq)
        `shouldBe` "Bool.True"

  describe "IEq/IOrd" $ do

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

  describe "HKT" $ do

    it "implementation of IEq for a where a is IXxx" $
      run ([r|
main = eq (Dict.IEq (eq,neq),Xxx,Xxx) where
  Dict.IEq (eq,neq) = ieq_ixxx dixxx_xxx      -- higher-kinded types (HKT)?
;

ieq_ixxx = func -> -- ixxx -> ieq
  Dict.IEq (eq,neq) where
    eq = func {f} ->  -- :: (ieq_xxx,a,a) -> Bool where a is IXxx
      eq (Dict.IEq (eq,neq), f ((f),x), f ((f),y)) where
        Dict.IEq (eq,neq) = dieq_bool
        (_,x,y) = ...
      ;
    ; where
      (f) = ...
    ;
  ;
;

dixxx_xxx = f where
  f = func -> -- :: (dixxx_xxx,X) -> Bool
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

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

prelude = iord_nat ++ ieq_nat ++ iord_bool ++ ieq_bool ++ iord ++ ieq ++ nat ++ bool

-- interface IEq(eq,neq)
ieq = [r|
  -- Methods are renamed to include "dict" param:
  --  - eq_  has a default implentation
  --  - neq_ has a default implentation
  dieq = Dict.IEq (eq,neq)  -- IEq is an interface with all members instantiated, so it support all types
  eq = func ->  -- (ieq_*,a,a) -> Bool
    case (x,y) of
      (~y,_) -> Bool.True
      _      -> Bool.False
    ; where
      (x,y) = ...
      -- AUTO
      ... = (p1,p2)
      Dict.IEq (eq,neq) = dieq
      (dieq,p1,p2) = ...
    ;
  ;
  neq = func ->  -- (ieq_*,a,a) -> Bool
    not (eq (dieq,x,y)) where
      (x,y) = ...
      -- AUTO
      ... = (p1,p2)
      Dict.IEq (eq,neq) = dieq
      (dieq,p1,p2) = ...
    ;
  ;
|]

-- interface IOrd(lt,lte,dt,gte)
iord = [r|
  -- lt_ = ???
  lte = func ->  -- (ieq_*,iord_*,a,a) -> Bool
    or ( lt (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y),
         eq (Dict.IEq (eq,neq),x,y) ) where
      (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y) = ...
    ;
  ;
  gt = func ->  -- (ieq_*,iord_*,a,a) -> Bool
    not (lte (dieq,diord,x,y)) where
      (x,y) = ...
      -- AUTO
      ... = (p1,p2)
      Dict.IEq (eq,neq) = dieq
      Dict.IOrd (lt,lte,gt,gte) = diord
      (dieq,diord,p1,p2) = ...
    ;
  ;
  gte = func ->  -- (ieq_*,iord_*,a,a) -> Bool
    or ( gt (dieq,diord,x,y),
         eq (dieq,x,y) ) where
      (x,y) = ...
      -- AUTO
      ... = (p1,p2)
      Dict.IEq (eq,neq) = dieq
      Dict.IOrd (lt,lte,gt,gte) = diord
      (dieq,diord,p1,p2) = ...
    ;
  ;
|]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- instance IEq (Bool)
ieq_bool = [r|
  -- Dict receives eq/neq methods.
  --  - implements eq, uses default neq
  --  - methods receive extra dict
  -- overrides default eq
  dieq_bool = Dict.IEq (eq,neq) where
    eq = func ->  -- (dieq_bool,Bool,Bool) -> Bool
      or (and (x,y), (and (not x, not y))) where
        (x,y) = ...
        -- AUTO
        ... = (p1,p2)
        (dieq,p1,p2) = ...
      ;
    ;
  ;
|]

-- implementation IOrd for Bool
iord_bool = [r|
  -- dict
  diord_bool = Dict.IOrd (lt,lte,gt,gte) where
    lt = func ->
      case (x,y) of
        (Bool.False, Bool.False) -> Bool.False
        (Bool.False, Bool.True)  -> Bool.True
        (Bool.True,  Bool.False) -> Bool.False
        (Bool.True,  Bool.True)  -> Bool.False
      ; where
        (x,y) = ...
        -- AUTO
        ... = (p1,p2)
        (dieq,diord,p1,p2) = ...
      ;
    ;
  ;
|]

-------------------------------------------------------------------------------

-- instance IEq (Int)
ieq_nat = [r|
  ieq_nat = Dict.IEq (eq,neq)
|]

-- implementation IOrd for Nat
iord_nat = [r|
  iord_nat = Dict.IOrd (lt,lte,gt,gte) where
    lt = func ->
      case (x,y) of
        (Nat.Zero,     Nat.Zero)     -> Bool.False
        (Nat.Zero,     _)            -> Bool.True
        (Nat.Succ _,   Nat.Zero)     -> Bool.False
        (Nat.Succ =x', Nat.Succ =y') -> lt (dieq,diord,x',y')
      ; where
        (x,y) = ...
        -- AUTO
        ... = (p1,p2)
        Dict.IOrd (lt,_,_,_) = diord
        (dieq,diord,p1,p2)   = ...
      ;
    ;
  ;
|]
