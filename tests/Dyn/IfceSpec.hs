{-# LANGUAGE QuasiQuotes #-}

module Dyn.IfceSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.PreludeIfce
import Dyn.Ifce
import qualified Dyn.Parser as P
import qualified Dyn.Eval   as E
import qualified Dyn.Decl   as D
import qualified Dyn.Order  as O

evalString    = E.evalStringF    (O.apply . D.apply . apply)
parseToString = P.parseToStringF (O.apply . D.apply . apply)

main :: IO ()
main = hspec spec

spec = do

  describe "IBounded" $ do

    it "main :: Bool = maximum;" $
      evalString ("main :: Bool = maximum' dIBoundedBool;" ++ bool_ibounded ++ bool ++ ibounded)
        `shouldBe` "Bool.True"

    it "(maximum,minimum)" $
      evalString ([r|
main = (x,y) where
  x :: Bool = maximum' dIBoundedBool;
  y :: Bool = minimum' dIBoundedBool;
.;
|] ++ bool_ibounded ++ bool ++ ibounded)
        `shouldBe` "(Bool.True,Bool.False)"

  describe "IEq" $ do

    it "IEq: eq" $
      evalString ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = (eq' dIEqBool) (Bool.False,Bool.False);
.;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: neq" $
      evalString ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = (neq' dIEqBool) (Bool.True,Bool.False);
.;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: default eq" $
      evalString ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = (neq' dIEqBool) ((eq' dIEqBool) (Bool.True,Bool.True), Bool.False);
.;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

  describe "IRec-IInd" $ do

    it "IInd" $
      evalString ([r|
main = (f' dIIndBool) Bool.True;

implementation of IInd for Bool with
  g :: (Bool -> ()) = func :: (Bool -> ()) -> () .;
.

interface IInd for a with
  g :: (a -> ());
.

f = func :: (a -> ()) where a is IInd ->
      (g' dIInda) ...
    .
;
|])
        `shouldBe` "()"

    it "IRec-rec" $
      evalString ([r|
main = (rec' dIRecNat) (Nat.Succ Nat.Zero);

implementation of IRec for Nat with
  rec :: (Nat -> ());
  rec = func :: (Nat -> ()) ->
    case ... of
      Nat.Zero    -> ();
      Nat.Succ =x -> (rec' dIRecNat) x;
    .
  .;
.

interface IRec for a with
  rec :: (a -> ());
.
|])
        `shouldBe` "()"

    it "IRec-ind" $
      evalString ([r|
main = (f' dIRecNat) (Nat.Succ Nat.Zero);

implementation of IRec for Nat with
  rec :: (Nat -> ());
  rec = func :: (Nat -> ()) ->
    case ... of
      Nat.Zero    -> ();
      Nat.Succ =x -> (rec' dIRecNat) x;
    .
  .;
.

interface IRec for a with
  rec :: (a -> ());
.

f :: (a -> ()) where a is IRec =
  func :: (a -> ()) where a is IRec ->
    (rec' dIReca) ...
  .
;
|])
        `shouldBe` "()"

  describe "IOrd" $ do

    it "IEq/IOrd" $
      evalString ([r|
main = (gt' (dIEqBool,dIOrdBool)) (Bool.False,Bool.True);
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "Bool.False"

    it "IEq/IOrd" $
      evalString ([r|
main = v where  -- (T<=F, T>=T, F>F, F<T)
  v = ( (lte' (dIEqBool,dIOrdBool)) (Bool.True,  Bool.False),
        (gte' (dIEqBool,dIOrdBool)) (Bool.True,  Bool.True ),
        (gt'  (dIEqBool,dIOrdBool)) (Bool.False, Bool.False),
        (lt'  (dIEqBool,dIOrdBool)) (Bool.False, Bool.True ) );
.;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.False,Bool.True,Bool.False,Bool.True)"

    it "IEq/IOrd/IAaa" $
      evalString ([r|
main = (f' (dIAaaBool,dIEqBool,dIOrdBool)) (Bool.True,Bool.False);

implementation of IAaa for Bool with
  f :: ((Bool,Bool) -> Bool) = func ->
    (g' (dIAaaBool,dIEqBool,dIOrdBool)) ...
  .;
.

interface IAaa for a where a is IOrd with
  f :: ((a,a) -> Bool);
.
g = func :: ((a,a) -> Bool) where a is IAaa ->
  (lt' (dIEqa,dIOrda)) ...
.;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "Bool.False"

    it "f a where a is IOrd" $
      evalString ([r|
main = ((f' (dIEqBool,dIOrdBool)) (Bool.True, Bool.False),
        (f' (dIEqBool,dIOrdBool)) (Bool.False,Bool.False));
f = func :: ((a,a) -> Bool) where a is IOrd ->
  (gt' (dIEqa,dIOrda)) ...
.;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.True,Bool.False)"

    it "TODO-dict-closure: ff1/ff2" $
      evalString ([r|
main = (ff1 (lte, (Bool.True,Bool.False)),
        ff2 (gte, (Bool.True,Bool.True )) ) where           -- gte must become closure
  ff1 :: ((((a,a)->Bool),(a,a)) -> Bool)
  ff1 = func -> f (x,y) where x::a y::a (f,(x,y))=... ;;

  ff2 :: ((((Bool,Bool)->Bool),(Bool,Bool)) -> Bool)        -- TODO: needs closure to hold actual dict
  ff2 = func -> f ps where (f,ps)=... ;;
;
|] ++ prelude)
        `shouldBe` "(Bool.False,Bool.True)"

    describe "TODO: impl-with-ctrs" $ do

    -- TODO: dIOrd(dIAaaXxx())
    it "TODO: implementation of IEq for a where a is IAaa" $
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
      x :: a
      y :: a
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

    it "TODO: implementation of IEq for Maybe of a" $
        evalString ([r|
main = (eq (m1,m1), neq(m1,m2))

m1 :: Maybe of () = Maybe.Nothing
m2 :: Maybe of () = Maybe.Just ()

--data Maybe of a
--data Maybe.Nothing
--data Maybe.Just with a

interface IEq for a with
  eq  :: ((a,a) -> Bool)
  neq :: ((a,a) -> Bool) = func :: ((a,a) -> Bool) ->
    not ((eq' dIEqa) ...)
  ;
;

implementation of IEq for () with
  eq = func :: (((),()) -> Bool) ->
    case ... of
      ((), ()) -> Bool.True
    ;
  ;
;

implementation of IEq for Maybe of a where (a is IEq) with
  eq = func :: ((Maybe of a,Maybe of a) -> Bool) ->
    case ... of
      (Maybe.Nothing, Maybe.Nothing) -> True
      (Maybe.Just =x, Maybe.Just =y) -> (eq' dIEqa) (x,y)
      _                              -> False
    ;
  ;
;
|])
        `shouldBe` "(Bool.True,Bool.True)"


  describe "Misc" $ do

    it "eq" $
      evalString ("main = (eq' dIEqChar) (Char.AA,Char.AA);"++char_ieq++char++nat++ieq++std)
        `shouldBe` "Bool.True"
    it "eq" $
      evalString ("main = (eq' dIEqChar) (Char.AA,Char.Aa);"++char_ieq++char++nat++ieq++std)
         `shouldBe` "Bool.False"
    it "gte" $
      evalString ("main = (gte' (dIEqChar,dIOrdChar)) (Char.AA,Char.Aa);"++prelude)
         `shouldBe` "Bool.False"
    it "gte" $
      evalString ("main = (gte' (dIEqNat,dIOrdNat)) (one,two);"++prelude)
         `shouldBe` "Bool.False"
    it "lt" $
      evalString ("main = (lt' (dIEqChar,dIOrdChar)) (Char.AA,Char.Aa);"++prelude)
         `shouldBe` "Bool.True"
    it "isLower" $
      evalString ("main = (isLower Char.BB, isLower Char.Bb);"++prelude)
         `shouldBe` "(Bool.False,Bool.True)"
    it "nextlet" $
      evalString ("main = (nextlet Char.Cc, nextlet Char.DD);"++prelude)
         `shouldBe` "(Char.Dd,Char.AA)"

  describe "dyn:" $ do
    it "f (toNat) True" $
      evalString ([r|
main = (f' dIEnumBool) Bool.True;

f :: (a -> Nat) where a is IEnum;
f = func :: (a -> Nat) where a is IEnum ->
  (toNat' dIEnuma) ...
.;
|] ++ prelude)
        `shouldBe` "(Nat.Succ Nat.Zero)"

    it "[(),True]" $
      evalString ([r|
main = (f' d) l where
  d = func ->
    case ... of
      (=k,=v) -> (getHash (ds_IEnum,k), v);
    .
  .;
.;

--data List of a is recursive
--data List.Nil
--data List.Cons with (a, List of a)

l :: List of IEnum;   -- a is dynamic IEnum
l = List.Cons ((Key.Bool, Bool.True),
    List.Cons ((Key.Unit, ()),
    List.Nil));

f = func :: (List of a -> List of Nat) where a is IEnum ->
  --let dIEnuma = ... in
  --  func {dIEnuma} ->
      case ... of
        List.Nil          -> List.Nil;
        List.Cons (=v,=l) -> List.Cons ((toNat' d') v', (f' dIEnuma) l) where
          (d',v') = dIEnuma v;
        .;
      .
.;
|] ++ unit_ienum ++ bool_ienum ++ ienum ++ std)
        `shouldBe` "(List.Cons ((Nat.Succ Nat.Zero),(List.Cons (Nat.Zero,List.Nil))))"

    it "succ [(),False]" $
      evalString ([r|
main = (f' gets) l where
  gets =
    func ->
      (getHash (ds_IEnum,k), v) where
        (k,v) = ...;
      .
    .
  ;
.;

--data List of a
--data List.Nil
--data List.Cons with (a, List of a)

l :: List of IEnum;   -- a is dynamic IEnum
l = List.Cons ((Key.Bool, Bool.False),
    List.Nil);

f = func :: (List of a -> List of Nat) where a is IEnum ->
  case ... of
    List.Nil          -> List.Nil;
    List.Cons (=v,=l) -> List.Cons ((succ' d') v', (f' gets) l) where
      (d',v') = gets v;
    .;
  .
.;
|] ++ unit_ienum ++ bool_ienum ++ ienum ++ std)
        `shouldBe` "(List.Cons (Bool.True,List.Nil))"

  describe "Lang" $ do
    it "toString" $
      evalString ([r|
main = ((toString' dIStringExpr) (Expr.Unit one), (toString' dIStringBool) Bool.True, (toString' dIStringExpr) (Expr.Var (one,zero)));

data Expr with Nat;
data Expr.Unit;
data Expr.Var with Nat;

interface IString for a with
  toString :: (a -> String);
.

implementation of IString for Expr.Unit with
  toString :: (Expr.Unit -> String);
  toString = func -> String.Unit (toStringExpr ...) . ;
.

implementation of IString for Bool with
  toString :: (Bool -> String);
  toString = func -> String.Bool . ;
.

implementation of IString for Expr.Var with
  toString :: (Expr.Var -> String);
  toString =
    func ->
      String.Var (toStringExpr ..., var) where
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
        `shouldBe` "((String.Unit (String.Pos (Nat.Succ Nat.Zero))),String.Bool,(String.Var ((String.Pos Nat.Zero),Nat.Zero)))"
