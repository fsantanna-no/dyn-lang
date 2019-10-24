{-# LANGUAGE QuasiQuotes #-}

module Dyn.Prelude where

import Text.RawString.QQ

prelude = iord_nat ++ ieq_nat ++ iord_bool ++ ieq_bool ++ iord ++ ieq ++ nat ++ bool

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

nat = [r|
  mul =
    func ->
      case ... of
        (_,  Nat.Zero)    -> Nat.Zero
        (=x, Nat.Succ =y) -> add (mul (x,y), x)
      ;
    ;

  add =
    func ->
      case ... of
        (=x, Nat.Zero)    -> x
        (=x, Nat.Succ =y) -> Nat.Succ (add (x,y))
      ;
    ;

  dec =
    func ->
      case ... of
        Nat.Succ =x -> x
      ;
    ;

  lte =
    func ->
      case ... of
        (Nat.Zero,_) -> Bool.True
        (_,Nat.Zero) -> Bool.False
        (Nat.Succ =x, Nat.Succ =y) -> lte (x,y)
      ;
    ;

  ten   = Nat.Succ nine
  nine  = Nat.Succ eight
  eight = Nat.Succ seven
  seven = Nat.Succ six
  six   = Nat.Succ five
  five  = Nat.Succ four
  four  = Nat.Succ three
  three = Nat.Succ two
  two   = Nat.Succ one
  one   = Nat.Succ zero
  zero  = Nat.Zero
|]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

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
      (_,x,y) = ...
    ;
  ;
  neq = func ->  -- (ieq_*,a,a) -> Bool
    not (eq (Dict.IEq (eq,neq),x,y)) where
      (Dict.IEq (eq,neq),x,y) = ...
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
    not (lte (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y)) where
      (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y) = ...
    ;
  ;
  gte = func ->  -- (ieq_*,iord_*,a,a) -> Bool
    or ( gt (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y),
         eq (Dict.IEq (eq,neq),x,y) ) where
      (Dict.IEq (eq,neq),Dict.IOrd (lt,lte,gt,gte),x,y) = ...
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
        (_,x,y) = ...
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
        (_,_,x,y) = ...
      ;
    ;
  ;
|]

-------------------------------------------------------------------------------

-- instance IEq (Int)
ieq_nat = [r|
  ieq_nat = Dict.IEq (eq,neq)
|]

-- implementation IOrd for Bool
iord_nat = [r|
  iord_nat = Dict.IOrd (lt,lte,gt,gte) where
    lt = func ->
      case (x,y) of
        (Nat.Zero,     Nat.Zero)     -> Bool.False
        (Nat.Zero,     _)            -> Bool.True
        (Nat.Succ _,   Nat.Zero)     -> Bool.False
        (Nat.Succ =x', Nat.Succ =y') -> lt (dieq,diord,x',y')
      ; where
        Dict.IOrd (lt,_,_,_) = diord
        (dieq,diord,x,y)  = ...
      ;
    ;
  ;
|]
