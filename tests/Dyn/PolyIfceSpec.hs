{-# LANGUAGE QuasiQuotes #-}

module Dyn.PolyIfceSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.PreludeIfce
import Dyn.Ifce

main :: IO ()
main = hspec spec

spec = do

  describe "IBounded" $ do

    it "XXX: main :: Bool = maximum;" $
      evalString ("main :: Bool = maximum' dIBoundedBool" ++ bool_ibounded ++ bool ++ ibounded)
        `shouldBe` "Bool.True"

    it "(maximum,minimum)" $
      evalString ([r|
main = (x,y) where
  x :: Bool = maximum' dIBoundedBool
  y :: Bool = minimum' dIBoundedBool
;
|] ++ bool_ibounded ++ bool ++ ibounded)
        `shouldBe` "(Bool.True,Bool.False)"

  describe "IEq" $ do

    it "IEq: eq" $
      evalString ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = eq (dBoolIEq, (Bool.False,Bool.False)) where
    Dict.IEq (eq,neq) = dBoolIEq
  ;
;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: neq" $
      evalString ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = neq (dBoolIEq, (Bool.True,Bool.False)) where
    Dict.IEq (eq,neq) = dBoolIEq
  ;
;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: default eq" $
      evalString ([r|  -- neq (eq(T,T), F)
main = x where
  x :: Bool = neq (dBoolIEq, (eq (dBoolIEq, (Bool.True,Bool.True)), Bool.False))
    where Dict.IEq (eq,neq) = dBoolIEq ;
;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

  describe "IRec-IInd" $ do

    it "IInd" $
      evalString ([r|
main = f (dBoolIInd,Bool.True) where
        Dict.IInd (g,f) = dBoolIInd
;

implementation of IInd for Bool with
  g :: (Bool -> ()) = func :: (Bool -> ()) -> () ;
;

interface IInd for a with
  g :: (a -> ())
  f :: (a -> ()) =
    func :: (a -> ()) ->
      g (daIInd, x) where
        x :: a = ...
      ;
    ;
;
|])
        `shouldBe` "()"

    it "IRec-rec" $
      evalString ([r|
main = rec (dNatIRec, (Nat.Succ Nat.Zero)) where
        Dict.IRec rec = dNatIRec
       ;

implementation of IRec for Nat with
  rec :: (Nat -> ())
  rec = func :: (Nat -> ()) ->
    case ... of
      Nat.Zero    -> ()
      Nat.Succ =x -> rec (daIRec,x) where
        x::Nat
        Dict.IRec rec = daIRec
      ;
    ;
  ;
;

interface IRec for a with
  rec :: (a -> ())
;
|])
        `shouldBe` "()"

    it "IRec-ind" $
      evalString ([r|
main = f (dNatIRec, (Nat.Succ Nat.Zero)) where
        Dict.IRec (rec,f) = dNatIRec
       ;

implementation of IRec for Nat with
  rec :: (Nat -> ())
  rec = func :: (Nat -> ()) ->
    case ... of
      Nat.Zero    -> ()
      Nat.Succ =x -> rec (daIRec,x) where
        x::Nat
        Dict.IRec (rec,f) = daIRec
      ;
    ;
  ;
;

interface IRec for a with
  rec :: (a -> ())
  f :: (a -> ())
  f = func :: (a -> ()) ->
    rec (daIRec,x) where
      x::a = ...
      Dict.IRec (rec,f) = daIRec
    ;
  ;
;
|])
        `shouldBe` "()"

  describe "IOrd" $ do

    it "IEq/IOrd" $
      evalString ([r|
main = gt ((dBoolIEq,dBoolIOrd), (Bool.False,Bool.True)) where
        Dict.IOrd (lt,lte,gt,gte) = dBoolIOrd
       ;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "Bool.False"

    it "IEq/IOrd" $
      evalString ([r|
main = v where  -- (T<=F, T>=T, F>F, F<T)
  v = ( lte ((dBoolIEq,dBoolIOrd), (Bool.True,  Bool.False)),
        gte ((dBoolIEq,dBoolIOrd), (Bool.True,  Bool.True )),
        gt  ((dBoolIEq,dBoolIOrd), (Bool.False, Bool.False)),
        lt  ((dBoolIEq,dBoolIOrd), (Bool.False, Bool.True )) ) where
        Dict.IOrd (lt,lte,gt,gte) = dBoolIOrd
  ;
;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.False,Bool.True,Bool.False,Bool.True)"

    it "IEq/IOrd/IAaa" $
      evalString ([r|
main = f ((dBoolIAaa,dBoolIEq,dBoolIOrd),(Bool.True,Bool.False)) where
        Dict.IAaa f = dBoolIAaa
       ;

implementation of IAaa for Bool with
  f :: ((Bool,Bool) -> Bool)
;

interface IAaa for a where a is IOrd with
  f :: ((a,a) -> Bool)
  f = func :: ((a,a) -> Bool) ->
    lt ((daIEq,daIOrd),(x,y)) where
      x::a
      y::a (x,y)=...
      Dict.IOrd (lt,lte,gt,gte) = daIOrd
    ;
  ;
;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "Bool.False"

    it "f a where a is IOrd" $
      evalString ([r|
main = (f ((dBoolIEq,dBoolIOrd),(Bool.True, Bool.False)),
        f ((dBoolIEq,dBoolIOrd),(Bool.False,Bool.False)))
f = func :: ((a,a) -> Bool) where a is IOrd ->
  gt ((daIEq,daIOrd),(x,y)) where
    x::a
    y::a
    (x,y)=...
    Dict.IOrd (lt,lte,gt,gte) = daIOrd
  ;
;
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
        evalString ("main = eq (dCharIEq, (Char.AA,Char.AA)) where Dict.IEq (eq,neq)=dCharIEq ;"++char_ieq++char++nat++ieq++std)
          `shouldBe` "Bool.True"
      it "eq" $
        evalString ("main = eq (dCharIEq, (Char.AA,Char.Aa)) where Dict.IEq (eq,neq)=dCharIEq ;"++char_ieq++char++nat++ieq++std)
           `shouldBe` "Bool.False"
      it "gte" $
        evalString ("main = gte ((dCharIEq,dCharIOrd),(Char.AA,Char.Aa)) where Dict.IOrd (lt,lte,gt,gte) = dCharIOrd ;"++prelude)

           `shouldBe` "Bool.False"
      it "lt" $
        evalString ("main = lt ((dCharIEq,dCharIOrd),(Char.AA,Char.Aa)) where Dict.IOrd (lt,lte,gt,gte) = dCharIOrd ;"++prelude)
           `shouldBe` "Bool.True"
      it "isLower" $
        evalString ("main = (isLower Char.BB, isLower Char.Bb)"++prelude)
           `shouldBe` "(Bool.False,Bool.True)"
      it "nextlet" $
        evalString ("main = (nextlet Char.Cc, nextlet Char.DD)"++prelude)
           `shouldBe` "(Char.Dd,Char.AA)"
