{-# LANGUAGE QuasiQuotes #-}

module Dyn.PolyIfceSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Parser
--import Dyn.Eval
import Dyn.Prelude
import Dyn.Ifce

main :: IO ()
main = hspec spec

spec = do

  describe "IBounded" $ do

    it "XXX: main = x where x::Bool = maximum;" $
      evalString True ("main = x where x::Bool = maximum;" ++ bool_ibounded ++ bool ++ ibounded)
        `shouldBe` "Bool.True"

    it "(maximum,minimum)" $
      evalString True ([r|
main = (x,y) where
  x :: Bool = maximum
  y :: Bool = minimum
;
|] ++ bool_ibounded ++ bool ++ ibounded)
        `shouldBe` "(Bool.True,Bool.False)"

  describe "IEq" $ do

    it "IEq: eq" $
      evalString True ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = eq (Bool.False,Bool.False)
;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: neq" $
      evalString True ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = neq (Bool.True,Bool.False)
;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: default eq" $
      evalString True ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = neq (eq (Bool.True,Bool.True), Bool.False)
;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

  describe "IRec-IInd" $ do

    it "IInd" $
      evalString True ([r|
main = f Bool.True

implementation of IInd for Bool with
  g :: (Bool -> ()) = func :: (Bool -> ()) -> () ;
;

interface IInd for a with
  g :: (a -> ())
  f :: (a -> ()) =
    func :: (a -> ()) ->
      g x where
        x :: a = ...
      ;
    ;
;
|])
        `shouldBe` "()"

    it "IRec-rec" $
      evalString True ([r|
main = rec (Nat.Succ Nat.Zero)

implementation of IRec for Nat with
  rec :: (Nat -> ())
  rec = func :: (Nat -> ()) ->
    case ... of
      Nat.Zero    -> ()
      Nat.Succ =x -> rec x where x::Nat;
    ;
  ;
;

interface IRec for a with
  rec :: (a -> ())
;
|])
        `shouldBe` "()"

    it "IRec-ind" $
      evalString True ([r|
main = f (Nat.Succ Nat.Zero)

implementation of IRec for Nat with
  rec :: (Nat -> ())
  rec = func :: (Nat -> ()) ->
    case ... of
      Nat.Zero    -> ()
      Nat.Succ =x -> rec x where x::Nat;
    ;
  ;
;

interface IRec for a with
  rec :: (a -> ())
  f :: (a -> ())
  f = func :: (a -> ()) -> rec x where x::a = ... ;;
;
|])
        `shouldBe` "()"

  describe "IOrd" $ do

    it "IEq/IOrd" $
      evalString True ([r|
main = gt (Bool.False,Bool.True)
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "Bool.False"

    it "IEq/IOrd" $
      evalString True ([r|
main = v where  -- (T<=F, T>=T, F>F, F<T)
  v = ( lte (Bool.True,  Bool.False),
        gte (Bool.True,  Bool.True ),
        gt  (Bool.False, Bool.False),
        lt  (Bool.False, Bool.True ) )
;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.False,Bool.True,Bool.False,Bool.True)"

    it "IEq/IOrd/IAaa" $
      evalString True ([r|
main = f (Bool.True,Bool.False)

implementation of IAaa for Bool with
  f :: ((Bool,Bool) -> Bool)
;

interface IAaa for a where a is IOrd with
  f :: ((a,a) -> Bool)
  f = func :: ((a,a) -> Bool) -> lt (x,y) where x::a y::a (x,y)=... ;;
;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "Bool.False"

    it "f a where a is IOrd" $
      evalString True ([r|
main = (f (Bool.True, Bool.False),
        f (Bool.False,Bool.False)) where
  f :: ((a,a) -> Bool) where a is IOrd
  f = func :: ((a,a) -> Bool) -> gt (x,y) where x::a y::a (x,y)=...;;
;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.True,Bool.False)"

    it "TODO-dict-closure: ff1/ff2" $
      evalString True ([r|
main = (ff1 (lte, (Bool.True,Bool.False)),
        ff2 (gte, (Bool.True,Bool.True )) ) where           -- gte must become closure
  ff1 :: ((((a,a)->Bool),(a,a)) -> Bool)
  ff1 = func -> f (x,y) where x::a y::a (f,(x,y))=... ;;

  ff2 :: ((((Bool,Bool)->Bool),(Bool,Bool)) -> Bool)        -- TODO: needs closure to hold actual dict
  ff2 = func -> f ps where (f,ps)=... ;;
;
|] ++ prelude)
        `shouldBe` "(Bool.False,Bool.True)"

    -- TODO: dIOrd(dIAaaXxx())
    it "TODO-impl-with-ctrs: implementation of IEq for a where a is IAaa" $
      evalString True ([r|
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

  describe "Misc" $ do

      it "eq" $
        evalString True ("main = eq (Char.AA,Char.AA)"++char_ieq++char++nat++ieq++std)
          `shouldBe` "Bool.True"
      it "eq" $
        evalString True ("main = eq (Char.AA,Char.Aa)"++char_ieq++char++nat++ieq++std)
           `shouldBe` "Bool.False"
      it "gte" $
        evalString True ("main = gte (Char.AA,Char.Aa)"++prelude)
           `shouldBe` "Bool.False"
      it "lt" $
        evalString True ("main = lt (Char.AA,Char.Aa)"++prelude)
           `shouldBe` "Bool.True"
      it "isLower" $
        evalString True ("main = (isLower Char.BB, isLower Char.Bb)"++prelude)
           `shouldBe` "(Bool.False,Bool.True)"
      it "capitalize" $
        evalString True ("main = (capitalize Char.CC, capitalize Char.Cc)"++prelude)
           `shouldBe` "(Char.CC,Char.CC)"
      it "nextlet" $
        evalString True ("main = (nextlet Char.Cc, nextlet Char.DD)"++prelude)
           `shouldBe` "(Char.Dd,Char.AA)"
