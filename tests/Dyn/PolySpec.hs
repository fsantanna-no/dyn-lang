{-# LANGUAGE QuasiQuotes #-}

module Dyn.PolySpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Prelude
import qualified Dyn.Parser  as P
import qualified Dyn.Eval    as E
import qualified Dyn.Analyse as A

parseToString = P.parseToStringF A.apply
evalString    = E.evalStringF A.apply

main :: IO ()
main = hspec spec

spec = do

  describe "IBounded" $ do

    it "main = x // x::Bool = maximum;" $
      evalString ("main = x;\nx::Bool = maximum;" ++ bool_ibounded ++ bool ++ ibounded)
        `shouldBe` "Bool.True"

    it "main = x where x::Bool = maximum;" $
      evalString ("main = x where x::Bool = maximum;.;" ++ bool_ibounded ++ bool ++ ibounded)
        `shouldBe` "Bool.True"

    it "(maximum,minimum)" $
      evalString ([r|
main = (x,y) where
  x :: Bool = maximum;
  y :: Bool = minimum;
.;
|] ++ bool_ibounded ++ bool ++ ibounded)
        `shouldBe` "(Bool.True,Bool.False)"

  describe "IEq" $ do

    it "IEq: eq" $
      evalString ([r|  -- neq (eq(T,T), F)
main = x where
  x = eq (Bool.False,Bool.False);
.;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: neq" $
      evalString ([r|  -- neq (eq(T,T), F)
main = x where
  x = neq (Bool.True,Bool.False);
.;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: default eq" $
      evalString ([r|  -- neq (eq(T,T), F)
main = x where
  x = neq (eq (Bool.True,Bool.True), Bool.False);
.;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

  describe "IEnum" $ do
    it "()" $ do
      evalString ("main :: () = fromNat zero;"++unit_ienum++ienum++nat)
        `shouldBe` "()"
    it "()" $ do
      evalString ("main = toNat ();"++unit_ienum++ienum++nat)
        `shouldBe` "Nat.Zero"
    it "succ" $ do
      evalString ("main = succ Bool.False;"++bool_ienum++ienum++nat)
        `shouldBe` "Bool.True"
    it "succ one" $ do
      evalString ("main = succ one;"++nat_ienum++ienum++nat++std)
        `shouldBe` "(Nat.Succ (Nat.Succ Nat.Zero))"

  describe "IRec-IInd" $ do

    it "IInd" $
      evalString ([r|
main = f Bool.True;

implementation of IInd for Bool with
  g = func -> () . ;
.

interface IInd for a with
  g :: (a -> ());
.

f :: (a -> ()) where a is IInd =
  func :: (a -> ()) where a is IInd ->
    g x where
      x = ...;
    .
  .
;
|])
        `shouldBe` "()"

    it "IRec-rec" $
      evalString ([r|
main = rec (Nat.Succ Nat.Zero);

implementation of IRec for Nat with
  rec = func ->
    case ... of
      Nat.Zero    -> ();
      Nat.Succ =x -> rec x;
    .
  .;
.

interface IRec for a with
  rec :: (a -> ());
.
|] ++ nat)
        `shouldBe` "()"

    it "IRec-ind" $
      evalString ([r|
main = f (Nat.Succ Nat.Zero);

implementation of IRec for Nat with
  rec = func ->
    case ... of
      Nat.Zero    -> ();
      Nat.Succ =x -> rec x;
    .
  .;
.

interface IRec for a with
  rec :: (a -> ());
.
f :: (a -> ()) where a is IRec =
  func :: (a -> ()) where a is IRec ->
    rec ...
  .
;
|] ++ nat)
        `shouldBe` "()"

  describe "IOrd" $ do

    it "IEq/IOrd" $
      evalString ([r|
main = gt (Bool.False,Bool.True);
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "Bool.False"

    it "IEq/IOrd" $
      evalString ([r|
main = v where  -- (T<=F, T>=T, F>F, F<T)
  v = ( lte (Bool.True,  Bool.False),
        gte (Bool.True,  Bool.True ),
        gt  (Bool.False, Bool.False),
        lt  (Bool.False, Bool.True ) );
.;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.False,Bool.True,Bool.False,Bool.True)"

    it "IEq/IOrd/IAaa" $
      evalString ([r|
main = f (Bool.True,Bool.False);

implementation of IAaa for Bool with
  f = func -> g (x,y) where x::Bool; y::Bool; (x,y)=...; . . ;
.

interface IAaa for a where a is IOrd with
  f :: ((a,a) -> Bool);
.
g :: ((a,a) -> Bool) where a is IAaa;
g = func :: ((a,a) -> Bool) where a is IAaa -> lt ... .;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "Bool.False"

    it "f a where a is IOrd" $
      evalString ([r|
main = (f (Bool.True, Bool.False),
        f (Bool.False,Bool.False));
f :: ((a,a) -> Bool) where a is IOrd;
f = func :: ((a,a) -> Bool) where a is IOrd ->
  gt (x,y) where
    (x,y)=...;
  .
.;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.True,Bool.False)"

    it "TODO-dict-closure: ff1/ff2" $
      evalString ([r|
main = (ff1 (lte, (Bool.True,Bool.False)),
        ff2 (gte, (Bool.True,Bool.True )) ) where           -- gte must become closure
  ff1 :: ((((a,a)->Bool),(a,a)) -> Bool)
  ff1 = func -> f (x,y) where (f,(x,y))=... ;;

  ff2 :: ((((Bool,Bool)->Bool),(Bool,Bool)) -> Bool)        -- TODO: needs closure to hold actual dict
  ff2 = func -> f ps where (f,ps)=... ;;
;
|] ++ prelude)
        `shouldBe` "(Bool.False,Bool.True)"

    -- TODO: dIOrd(dIAaaXxx())
    it "TODO-impl-with-ctrs: implementation of IEq for a where a is IAaa" $
      evalString ([r|
main = (lt (Xxx.True,Xxx.False), gt (Xxx.True,Xxx.False))

implementation of IEq for a where a is IAaa with
  eq = func :: ((a,a) -> Bool) ->
    matches ...
  ;
;

implementation of IOrd for a where a is IAaa with
  lt :: ((a,a) -> Bool)
  lt = func :: ((a,a) -> Bool) ->
    lt (f x, f y) where
      (x,y) = ...
    ;
  ;
;

interface IAaa for a with
  f :: (a -> Bool)
;

implementation of IAaa for Xxx with
  f :: (Xxx -> Bool)
  f = func :: (Xxx -> Bool) ->
    case ... of
      Xxx.True  -> Bool.True
      Xxx.False -> Bool.False
    ;
  ;
;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq ++ std)
        `shouldBe` "(Bool.False,Bool.True)"

  describe "Misc" $ do

    it "eq" $
      evalString ("main = eq (Char.AA,Char.AA);"++char_ieq++char++nat++ieq++std)
        `shouldBe` "Bool.True"
    it "eq" $
      evalString ("main = eq (Char.AA,Char.Aa);"++char_ieq++char++nat++ieq++std)
         `shouldBe` "Bool.False"
    it "gte" $
      evalString ("main = gte (Char.AA,Char.Aa);"++prelude)
         `shouldBe` "Bool.False"
    it "lt" $
      evalString ("main = lt (Char.AA,Char.Aa);"++prelude)
         `shouldBe` "Bool.True"
    it "isLower" $
      evalString ("main = (isLower Char.BB, isLower Char.Bb);"++prelude)
         `shouldBe` "(Bool.False,Bool.True)"
    it "capitalize" $
      evalString ("main = (capitalize Char.CC, capitalize Char.Cc);"++prelude)
         `shouldBe` "(Char.CC,Char.CC)"
    it "nextlet" $
      evalString ("main = (nextlet Char.Cc, nextlet Char.DD);"++prelude)
         `shouldBe` "(Char.Dd,Char.AA)"

  describe "dyn:" $ do

    it "f (toNat) True" $
      evalString ([r|
main = f Bool.True;

f :: (a -> Nat) where a is IEnum;
f = func :: (a -> Nat) where a is IEnum ->
  toNat ...
.;
|] ++ prelude)
        `shouldBe` "(Nat.Succ Nat.Zero)"

    it "f [False,True]" $
      evalString ([r|
main = f l;

l :: List of Bool;
l = List.Cons (Bool.False,
    List.Cons (Bool.True,
    List.Nil));

f :: (List of a -> List of Nat) where a is IEnum;
f = func :: (List of a -> List of Nat) where a is IEnum ->
  case ... of
    List.Nil          -> List.Nil;
    List.Cons (=x,=l) -> List.Cons (toNat x, f l) where x::a; l::List of a; .;
  .
.;
|] ++ prelude)
        `shouldBe` "(List.Cons (Nat.Zero,(List.Cons ((Nat.Succ Nat.Zero),List.Nil))))"

    it "f [one,zero]" $
      evalString ([r|
main = f l;

l :: List of Nat;
l = List.Cons (one,
    List.Cons (zero,
    List.Nil));

f :: (List of a -> List of Nat) where a is IEnum;
f = func :: (List of a -> List of Nat) where a is IEnum ->
  case ... of
    List.Nil          -> List.Nil;
    List.Cons (=x,=l) -> List.Cons (toNat x, f l) where x::a; l::List of a; .;
  .
.;
|] ++ prelude)
        `shouldBe` "(List.Cons ((Nat.Succ Nat.Zero),(List.Cons (Nat.Zero,List.Nil))))"

    it "[(),True]" $
      evalString ([r|
main = f l1;

l1 :: List of IEnum;
l1 = List.Cons (Bool.True, l2);

l2 :: List of IEnum;
l2 = List.Cons ((), List.Nil);

f = func :: (List of a -> List of Nat) where a is IEnum ->
  case ... of
    List.Nil          -> List.Nil;
    List.Cons (=v,=l) -> List.Cons (toNat v, f l);
  .
.;
|] ++ unit_ienum ++ bool_ienum ++ ienum ++ std)
        `shouldBe` "(List.Cons ((Nat.Succ Nat.Zero),(List.Cons (Nat.Zero,List.Nil))))"

    it "[(),True,one]" $
      evalString ([r|
main = f l;

l :: List of IEnum;
l = List.Cons (Bool.True,
    List.Cons ((),
    List.Cons (one,
    List.Nil)));

f = func :: (List of a -> List of Nat) where a is IEnum ->
  case ... of
    List.Nil          -> List.Nil;
    List.Cons (=v,=l) -> List.Cons (toNat v, f l);
  .
.;
|] ++ nat_ienum ++ char_ieq ++ unit_ienum ++ bool_ienum ++ ienum ++ ieq ++ nat ++ std)
        `shouldBe` "(List.Cons ((Nat.Succ Nat.Zero),(List.Cons (Nat.Zero,(List.Cons ((Nat.Succ Nat.Zero),List.Nil))))))"

    it "succ [(),False]" $
      evalString ([r|
main = f l;

l :: List of IEnum;   -- a is dynamic IEnum
l = List.Cons (Bool.False,
    List.Nil);

f = func :: (List of a -> List of Nat) where a is IEnum ->
  case ... of
    List.Nil          -> List.Nil;
    List.Cons (=v,=l) -> List.Cons (succ v, f l);
  .
.;
|] ++ prelude)
        `shouldBe` "(List.Cons (Bool.True,List.Nil))"

  describe "Lang" $ do
    it "XXX: toString" $
      evalString ([r|
main = (toString (Expr.Unit one), toString (Expr.Var (zero,zero)));
data Expr with Nat;
data Expr.Unit;
data Expr.Var with Nat;

interface IString for a with
  toString :: (a -> String);
.

implementation of IString for Expr.Unit with
  toString :: (Expr.Unit -> String);
  toString = func -> (toStringExpr ..., String.Unit) . ;
.

implementation of IString for Expr.Var with
  toString :: (Expr.Unit -> String);
  toString =
    func ->
      (toStringExpr ..., var) where
        Expr.Var (_, var) = ...;
      .
    .
  ;
.

toStringExpr :: (Expr -> String);
toStringExpr =
  func ->
    let Expr n = ...; in
      String.Pos n
    .
  .
;
|] ++ nat ++ std)
        `shouldBe` "((Expr.Unit (Nat.Succ Nat.Zero)),(Expr.Var (Nat.Zero,Nat.Zero)))"


