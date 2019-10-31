{-# LANGUAGE QuasiQuotes #-}

module Dyn.Prelude where

import Text.RawString.QQ

prelude = iord_nat  ++ ieq_nat
       ++ iord_bool ++ ieq_bool ++ ibounded_bool
       ++ iord      ++ ieq      ++ ibounded
       ++ std       ++ nat      ++ bool

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

std = [r|
  matches :: ((a,a) -> Bool)
  matches = func ->
    case (x,y) of
      (~y,_) -> Bool.True
      _      -> Bool.False
    ; where
      (x,y) = ...
    ;
  ;
|]

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
  thousand = mul (ten,ten)
  hundred  = mul (ten,ten)
  ten      = Nat.Succ nine
  nine     = Nat.Succ eight
  eight    = Nat.Succ seven
  seven    = Nat.Succ six
  six      = Nat.Succ five
  five     = Nat.Succ four
  four     = Nat.Succ three
  three    = Nat.Succ two
  two      = Nat.Succ one
  one      = Nat.Succ zero
  zero     = Nat.Zero

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

  sub =
    func ->
      case ... of
        (=x, Nat.Zero)             -> x
        (Nat.Succ =x, Nat.Succ =y) -> sub (x,y)
      ;
    ;

  dec =
    func ->
      case ... of
        Nat.Succ =x -> x
      ;
    ;

  rem =
    func ->
      case lt (x,y) of
        Bool.True  -> x
        Bool.False -> rem (sub (x,y), y)
      ; where
        x :: Nat
        y :: Nat
        (x,y) = ...
      ;
    ;

  nlte =
    func ->
      case ... of
        (Nat.Zero,_) -> Bool.True
        (_,Nat.Zero) -> Bool.False
        (Nat.Succ =x, Nat.Succ =y) -> nlte (x,y)
      ;
    ;
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
    eq  :: ((a,a) -> Bool)
    neq :: ((a,a) -> Bool) = func :: ((a,a) -> Bool) ->
      not (eq (x,y)) where
        x :: a
        y :: a
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
      or (lt (x,y), eq (x,y)) where
        x :: a
        y :: a
        (x,y) = ...
      ;
    ;
    gt = func :: ((a,a) -> Bool) ->
      not (lte (x,y)) where
        x :: a
        y :: a
        (x,y) = ...
      ;
    ;
    gte = func :: ((a,a) -> Bool) ->
      or (gt (x,y), eq (x,y)) where
        x :: a
        y :: a
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
        x :: Bool
        y :: a
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
        x :: Bool
        y :: a
        (x,y) = ...
      ;
    ;
  ;
|]

ieq_nat = [r|
  implementation of IEq for Nat with
    eq = func :: ((Nat,Nat) -> Bool) ->
      matches ...
    ;
  ;
|]

iord_nat = [r|
  implementation of IOrd for Nat with
    lt = func :: ((Nat,Nat) -> Bool) ->
      case (x,y) of
        (Nat.Zero,     Nat.Zero)     -> Bool.False
        (Nat.Zero,     _)            -> Bool.True
        (Nat.Succ _,   Nat.Zero)     -> Bool.False
        (Nat.Succ =x', Nat.Succ =y') -> lt (x',y')    -- TODO: lt recursive call
      ; where
        x' :: Nat
        y' :: Nat
        (x,y) = ...
      ;
    ;
  ;
|]
