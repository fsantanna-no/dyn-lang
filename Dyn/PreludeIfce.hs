{-# LANGUAGE QuasiQuotes #-}

module Dyn.PreludeIfce where

import Text.RawString.QQ

prelude = nat_iord   ++ nat_ieq
       ++ bool_ienum ++ bool_iord ++ bool_ieq ++ bool_ibounded
       ++ char_iord  ++ char_ieq
       ++ unit_ienum
       ++ nat        ++ char
       ++ ienum      ++ iord      ++ ieq      ++ ibounded
       ++ std        ++ bool

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

std = [r|
  data List of a;
  data List.Nil;
  data List.Cons with (a, List of a);

  matches :: ((a,a) -> Bool);
  matches = func {
    case (x,y) {
      (~y,_) -> Bool.True;
      _      -> Bool.False;
    } where {
      (x,y) = ...;
    }
  };

  getHash = func {
    let (dicts,key) = ...; {
      case dicts {
        List.Cons ((~key,=dict),_) -> dict;
        List.Cons (_,=dicts')      -> getHash (dicts',key);
      }
    }
  };
|]

ibounded = [r|
  interface IBounded for a {
    minimum :: a;
    maximum :: a;
  }
|]

ieq = [r|
  interface IEq for a {
    eq  :: ((a,a) -> Bool);
  }
  neq = func :: ((a,a) -> Bool) where a is IEq {
    not ((eq' dIEqa) ...)
  };
|]

iord = [r|
  interface IOrd for a where a is IEq {
    lt  :: ((a,a) -> Bool);
  }
  lte = func :: ((a,a) -> Bool) where a is IOrd {
    or ((lt' (dIEqa,dIOrda)) ..., (eq' dIEqa) ...)
  };
  gt = func :: ((a,a) -> Bool) where a is IOrd {
    not ((lte' (dIEqa,dIOrda)) ...)
  };
  gte = func :: ((a,a) -> Bool) where a is IOrd {
    or ((gt' (dIEqa,dIOrda)) ..., (eq' dIEqa) ...)
  };
|]

ienum = [r|
  interface IEnum for a {
    toNat   :: (a -> Nat);
    fromNat :: (Nat -> a);
  }
  succ = func :: (a -> a) where a is IEnum {
    (fromNat' dIEnuma) (Nat.Succ ((toNat' dIEnuma) ...))
  };
|]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

unit_ienum = [r|
  implementation of IEnum for () {
    toNat = func :: (() -> Nat) {
      case ... {
        () -> Nat.Zero;
      }
    };

    fromNat = func :: (Nat -> ()) {
      case ... {
        Nat.Zero -> ();
      }
    };
  }
|]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Bool type: not, and, or
bool = [r|
  not = func {
    case ... {
      Bool.False -> Bool.True;
      Bool.True  -> Bool.False;
    }
  };

  and = func {
    case ... {
      (Bool.False, _) -> Bool.False;
      (_, Bool.False) -> Bool.False;
      _               -> Bool.True;
    }
  };

  or = func {
    case ... {
      (Bool.True, _)  -> Bool.True;
      (_,         =y) -> y;
    }
  };
|]

bool_ibounded = [r|
  implementation of IBounded for Bool {
    minimum :: Bool = Bool.False;
    maximum :: Bool = Bool.True;
  }
|]

bool_ieq = [r|
  implementation of IEq for Bool {
    eq = func :: ((Bool,Bool) -> Bool) {
      or (and (x,y), (and (not x, not y))) where {
        (x,y) = ...;
      }
    };
  }
|]

bool_iord = [r|
  implementation of IOrd for Bool {
    lt = func :: ((Bool,Bool) -> Bool) {
      case ... {
        (Bool.False, Bool.False) -> Bool.False;
        (Bool.False, Bool.True)  -> Bool.True;
        (Bool.True,  Bool.False) -> Bool.False;
        (Bool.True,  Bool.True)  -> Bool.False;
      }
    };
  }
|]

bool_ienum = [r|
  implementation of IEnum for Bool {
    toNat = func :: (Bool -> Nat) {
      case ... {
        Bool.False -> Nat.Zero;
        Bool.True  -> Nat.Succ Nat.Zero;
      }
    };

    fromNat = func :: (Nat -> Bool) {
      case ... {
        Nat.Zero          -> Bool.False;
        Nat.Succ Nat.Zero -> Bool.True;
      }
    };
  }
|]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

char = [r|
  --data Char
  --data Char.AA
  --data Char.BB
  --data Char.CC
  --data Char.DD
  --data Char.Aa
  --data Char.Bb
  --data Char.Cc
  --data Char.Dd

  ord :: (Char -> Nat) = func {
    case ... {
      Char.AA -> one;
      Char.BB -> two;
      Char.CC -> three;
      Char.DD -> four;
      Char.Aa -> add (ten, one);
      Char.Bb -> add (ten, two);
      Char.Cc -> add (ten, three);
      Char.Dd -> add (ten, four);
    }
  };

  chr :: (Nat -> Char) = func {
    case ... {
      ~ one                -> Char.AA;
      ~ two                -> Char.BB;
      ~ three              -> Char.CC;
      ~ four               -> Char.DD;
      ~ (add (ten, one))   -> Char.Aa;
      ~ (add (ten, two))   -> Char.Bb;
      ~ (add (ten, three)) -> Char.Cc;
      ~ (add (ten, four))  -> Char.Dd;
    }
  };

  isLower :: (Char -> Bool) = func {
    and ((gte' (dIEqChar,dIOrdChar)) (c,Char.Aa), (lte' (dIEqChar,dIOrdChar)) (c,Char.Dd)) where {
      c :: Char;
      c = ...;
    }
  };

  capitalize :: (Char -> Char) = func {
    case isLower c {
      Bool.True  -> chr (sub (ord c, off));
      Bool.False -> c;
    } where {
      c :: Char;
      c = ...;
      off :: Nat = sub (ord Char.Aa, ord Char.AA);
    }
  };

  nextlet :: (Char -> Char) = func {
    let c = ...; {
      chr (add (rem (add (sub (ord c,min), one),
                     add (sub (max,min), one)),
           min))
       --return chr (((((ord c) - min) + 1) rem ((max-min)+1)) + min)
      where {
        (min,max) = case isLower c {
          Bool.True  -> (ord Char.Aa, ord Char.Dd);
          Bool.False -> (ord Char.AA, ord Char.DD);
        };
      }
    }
  };
|]

char_ieq = [r|
  implementation of IEq for Char {
    eq = func :: ((Char,Char) -> Bool) {
      matches ...
    };
  }
|]

char_iord = [r|
  implementation of IOrd for Char {
    lt = func :: ((Char,Char) -> Bool) {
      (lt' (dIEqNat,dIOrdNat)) (ord x, ord y) where {
        (x,y) = ...;
      }
    };
  }
|]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

nat = [r|
  thousand = mul (ten,ten);
  hundred  = mul (ten,ten);
  ten      = Nat.Succ nine;
  nine     = Nat.Succ eight;
  eight    = Nat.Succ seven;
  seven    = Nat.Succ six;
  six      = Nat.Succ five;
  five     = Nat.Succ four;
  four     = Nat.Succ three;
  three    = Nat.Succ two;
  two      = Nat.Succ one;
  one      = Nat.Succ zero;
  zero     = Nat.Zero;

  mul =
    func {
      case ... {
        (_,  Nat.Zero)    -> Nat.Zero;
        (=x, Nat.Succ =y) -> add (mul (x,y), x);
      }
    }
  ;

  add =
    func {
      case ... {
        (=x, Nat.Zero)    -> x;
        (=x, Nat.Succ =y) -> Nat.Succ (add (x,y));
      }
    }
  ;

  sub =
    func {
      case ... {
        (=x, Nat.Zero)             -> x;
        (Nat.Succ =x, Nat.Succ =y) -> sub (x,y);
      }
    }
  ;

  dec =
    func {
      case ... {
        Nat.Succ =x -> x;
      }
    }
  ;

  rem =
    func {
      case (lt' (dIEqNat,dIOrdNat)) (x,y) {
        Bool.True  -> x;
        Bool.False -> rem (sub (x,y), y);
      } where {
        (x,y) = ...;
      }
    }
  ;

  nlte =
    func {
      case ... {
        (Nat.Zero,_) -> Bool.True;
        (_,Nat.Zero) -> Bool.False;
        (Nat.Succ =x, Nat.Succ =y) -> nlte (x,y);
      }
    }
  ;
|]

nat_ieq = [r|
  implementation of IEq for Nat {
    eq = func :: ((Nat,Nat) -> Bool) {
      matches ...
    };
  }
|]

nat_iord = [r|
  implementation of IOrd for Nat {
    lt = func :: ((Nat,Nat) -> Bool) {
      case ... {
        (Nat.Zero,     Nat.Zero)     -> Bool.False;
        (Nat.Zero,     _)            -> Bool.True;
        (Nat.Succ _,   Nat.Zero)     -> Bool.False;
        (Nat.Succ =x', Nat.Succ =y') -> (lt' (dIEqNat,dIOrdNat)) (x',y');
      }
    };
  }
|]
