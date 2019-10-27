{-# LANGUAGE QuasiQuotes #-}

module Dyn.Prelude where

import Text.RawString.QQ

prelude = iord_nat ++ iord_bool ++ ieq_bool ++ ibounded_bool ++ iord ++ ieq ++ ibounded ++ nat ++ bool

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

ibounded = [r|
  interface IBounded for a with
    minimum :: a
    maximum :: a
  ;
|]

ieq = [r|
  interface IEq for a with
    eq :: ((a,a) -> Bool) = func :: ((a,a) -> Bool) ->
      case (x,y) of
        (~y,_) -> Bool.True
        _      -> Bool.False
      ; where
        (x,y) = ...
      ;
    ;
    neq :: ((a,a) -> Bool) = func :: ((a,a) -> Bool) ->
      not (eq (daIEq,(x,y))) where
        (x,y) = ...
      ;
    ;
  ;
|]

iord = [r|
  interface IOrd for a where a is IEq with
    lt  :: ((a,a) -> Bool)
    lte :: ((a,a) -> Bool)
    gt  :: ((a,a) -> Bool)
    gte :: ((a,a) -> Bool)

    lte = func :: ((a,a) -> Bool) ->
      or ( lt ((daIEq,daIOrd),(x,y)),
           eq (daIEq,(x,y)) ) where
        (x,y) = ...
      ;
    ;
    gt = func :: ((a,a) -> Bool) ->
      not (lte ((daIEq,daIOrd),(x,y))) where
        (x,y) = ...
      ;
    ;
    gte = func :: ((a,a) -> Bool) ->
      or ( gt ((daIEq,daIOrd),(x,y)),
           eq (daIEq,(x,y)) ) where
        (x,y) = ...
      ;
    ;
  ;
|]

-------------------------------------------------------------------------------

ibounded_bool = [r|
  implementation of IBounded for Bool with
    minimum :: Bool = Bool.False
    maximum :: Bool = Bool.True
  ;
|]

ieq_bool = [r|
  implementation of IEq for Bool with
    eq = func :: ((Bool,Bool) -> Bool) ->
      or (and (x,y), (and (not x, not y))) where
        (x,y) = ...
      ;
    ;
  ;
|]

iord_bool = [r|
  implementation of IOrd for Bool with
    lt = func :: ((Bool,Bool) -> Bool) ->
      case (x,y) of
        (Bool.False, Bool.False) -> Bool.False
        (Bool.False, Bool.True)  -> Bool.True
        (Bool.True,  Bool.False) -> Bool.False
        (Bool.True,  Bool.True)  -> Bool.False
      ; where
        (x,y) = ...
      ;
    ;
  ;
|]

iord_nat = [r|
  implementation of IOrd for Nat with
    lt = func :: ((Bool,Bool) -> Bool) ->
      case (x,y) of
        (Nat.Zero,     Nat.Zero)     -> Bool.False
        (Nat.Zero,     _)            -> Bool.True
        (Nat.Succ _,   Nat.Zero)     -> Bool.False
        (Nat.Succ =x', Nat.Succ =y') -> lt ((daIEq,daIOrd),(x',y'))
      ; where
        (x,y) = ...
      ;
    ;
  ;
|]
