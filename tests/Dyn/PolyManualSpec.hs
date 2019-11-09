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

  describe "IBounded" $ do

    it "main :: Bool = maximum;" $
      evalString ("main = maximum' dIBoundedBool" ++ bool_ibounded ++ bool ++ ibounded)
        `shouldBe` "Bool.True"

    it "(maximum,minimum)" $
      evalString ([r|
main = (maximum' dIBoundedBool,minimum' dIBoundedBool)
|] ++ bool_ibounded ++ bool ++ ibounded)
        `shouldBe` "(Bool.True,Bool.False)"

  describe "IEq" $ do

    it "IEq: default eq" $
      evalString ([r|  -- neq (eq(T,T), F)
main = (neq' dIEq) ((eq' dIEq)(Bool.True,Bool.True), Bool.False)
|] ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "IEq: (eq ((T,F),(F,T)), eq ((T,F),(T,F))" $
      evalString ([r|
main = (x,y) where
  x = (eq' dIEq) ((Bool.True,Bool.False), (Bool.False,Bool.True))
  y = (eq' dIEq) ((Bool.True,Bool.False), (Bool.True,Bool.False))
;
|] ++ bool ++ ieq)
        `shouldBe` "(Bool.False,Bool.True)"

    it "IEq: overrides eq (ieq_Bool)" $
      evalString ([r|
main = v where  -- neq (eq(T,T), F)
  v = (neq' dIEqBool) ((eq' dIEqBool) (Bool.True,Bool.True), Bool.False)
;
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

  describe "IEq/IOrd" $ do

    it "IEq/IOrd" $
      evalString ([r|
main = v where  -- (T<=F, T>=T, F>F, F<T)
  v = ( (lte' (dIEqBool,dIOrdBool)) (Bool.True, Bool.False),
        (gte' (dIEqBool,dIOrdBool)) (Bool.True, Bool.True),
        (gt'  (dIEqBool,dIOrdBool)) (Bool.False,Bool.False),
        (lt'  (dIEqBool,dIOrdBool)) (Bool.False,Bool.True) )
;
|] ++ bool_iord ++ bool_ieq ++ bool ++ iord ++ ieq)
        `shouldBe` "(Bool.False,Bool.True,Bool.False,Bool.True)"

  describe "impl w/ extra ctrs" $ do

    it "TODO: implementation of IEq for a where a is IXxx" $
      evalString ([r|
main = (eq' (dIEqIXxx dIXxxXxx)) (Xxx,Xxx)

dIEqIXxx = Dict.IEq (eq) where
  eq = func {f} ->  -- :: (ieq_xxx,a,a) -> Bool where a is IXxx
    eq (Dict.IEq (eq), f ((f),x), f ((f),y)) where
      Dict.IEq (eq) = dIEqBool
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
|] ++ bool_ieq ++ bool ++ ieq)
        `shouldBe` "Bool.True"

    it "f = func :: ((a -> Int) where a is IEq) -> eq (x,x)" $
      evalString ([r|
main = (f dIEqNat) one
f = func ->
  let dIEqa = ... in
    func {dIEqa} ->
      (eq' dIEqa) (x,x) where
        x = ...
      ;
    ;
  ;
;
|] ++ prelude)
          `shouldBe` "Bool.True"

  describe "ienum" $ do

    it "ienum: ()" $
      evalString ([r|
main = (toNat' dIEnumUnit) ()
|] ++ unit_ienum ++ ienum)
        `shouldBe` "Nat.Zero"

    it "ienum: Bool" $
      evalString ([r|
main = (toNat' dIEnumBool) Bool.True
|] ++ bool_ienum ++ ienum)
        `shouldBe` "(Nat.Succ Nat.Zero)"

    it "succ: Bool" $
      evalString ([r|
main = (succ' dIEnumBool) Bool.False
|] ++ bool_ienum ++ ienum)
        `shouldBe` "Bool.True"

  describe "dynamic" $ do

    it "succ: Bool" $
      evalString ([r|
main = (succ'2 dicts) dynv
dynv :: Bool = (Key.Bool,Bool.False)  -- :: IEnum::Bool
dicts = List.Cons ((Key.Bool,dIEnumBool), List.Nil)

succ'2 = func ->
  let ds = ... in
    func {ds} ->
      let (k,v) = ... in
        (fromNat' (getDict (ds,k))) (Nat.Succ ((toNat' (getDict (ds,k))) v))
      ;
    ;
  ;
;
|] ++ bool_ienum ++ ienum ++ std)
        `shouldBe` "Bool.True"

    it "[(),True]" $
      evalString ([r|
main = (f' dict) l

dict = List.Cons ((Key.XXX, dIEnumUnit),
       List.Cons ((Key.YYY, dIEnumBool),
       List.Nil))

--data List for a
--data List.Nil
--data List.Cons with (a, List of a)

l :: List of a where a is IEnum   -- a is dynamic IEnum
l = List.Cons ((Key.YYY, Bool.True),
    List.Cons ((Key.XXX, ()),
    List.Nil))

f :: (List of a -> List of Nat) where a is IEnum  -- a is dynamic IEnum
f' = func :: (List of a -> List of Nat) where a is IEnum ->
  let dsa = ... in
    func {dsa} ->
      case ... of
        List.Nil                 -> List.Nil
        List.Cons ((=key,=x),=l) -> List.Cons ((toNat' (getDict (dsa,key))) x, (f' dict) l) where
          key :: Key
          x   :: a
          l   :: List of a
        ;
      ;
    ;
  ;
;
|] ++ unit_ienum ++ bool_ienum ++ ienum ++ std)
        `shouldBe` "(List.Cons ((Nat.Succ Nat.Zero),(List.Cons (Nat.Zero,List.Nil))))"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

prelude = unit_ienum ++ nat_ieq ++ bool_ienum ++ bool_iord ++ bool_ieq ++ iord ++ ieq ++ nat ++ bool ++ ienum ++ std

std = [r|
  getDict = func ->
    let (dicts,key) = ... in
      case dicts of
        List.Cons ((~key,=dict),_) -> dict
        List.Cons (_,=dicts')      -> getDict (dicts',key)
      ;
    ;
  ;
|]

-- interface IBounded(minimum,maximum)
ibounded = [r|
  minimum' = func ->
    minimum ... where
      Dict.IBounded (minimum,_) = ...
    ;
  ;
  maximum' = func ->
    maximum ... where
      Dict.IBounded (_,maximum) = ...
    ;
  ;
|]

ienum = [r|
  toNat' = func ->
    toNat where
      Dict.IEq (toNat,_) = ...
    ;
  ;
  fromNat' = func ->
    fromNat where
      Dict.IEq (_,fromNat) = ...
    ;
  ;
  succ' = func ->
    let dict = ... in
      func {dict} ->
        (fromNat' dict) (Nat.Succ ((toNat' dict) ...))
      ;
    ;
  ;
|]

ieq = [r|
  eq' = func ->
    eq ... where
      Dict.IEq (eq) = ...
    ;
  ;
  neq' = func ->
    neq_IEq ...
  ;

  dIEq = Dict.IEq (eq_IEq)

  neq_IEq = func ->
    let dIEqa = ... in
      func {dIEqa} ->
        not ((eq' dIEqa) ...)
      ;
    ;
  ;

  eq_IEq = func ->
    let dIEqa = ... in
      func {dIEqa} ->
        let (x,y) = ... in
          case (x,y) of
            (~y,_) -> Bool.True
            _      -> Bool.False
          ;
        ;
      ;
    ;
  ;
|]

-- interface IOrd(lt,lte,gt,gte)
iord = [r|
  lte_IOrd = func ->
    let (dIEqa,dIOrda) = ...  in
      func {dIEqa,dIOrda} ->
        or ( (lt' (dIEqa,dIOrda)) ...,
             (eq' dIEqa)          ... )
      ;
    ;
  ;
  gt_IOrd = func ->
    let (dIEqa,dIOrda) = ...  in
      func {dIEqa,dIOrda} ->
        not ((lte' (dIEqa,dIOrda)) ...)
      ;
    ;
  ;
  gte_IOrd = func ->
    let (dIEqa,dIOrda) = ...  in
      func {dIEqa,dIOrda} ->
        or ( (gt' (dIEqa,dIOrda)) ...,
             (eq' dIEqa)          ... )
        ;
    ;
  ;

  -- lt,lte,gt,gte
  lt' = func ->
    lt ... where
      (_, Dict.IOrd (lt,_,_,_)) = ...
    ;
  ;
  lte' = func ->
    lte ... where
      (_, Dict.IOrd (_,lte,_,_)) = ...
    ;
  ;
  gt' = func ->
    gt ... where
      (_, Dict.IOrd (_,_,gt,_)) = ...
    ;
  ;
  gte' = func ->
    gte ... where
      (_, Dict.IOrd (_,_,_,gte)) = ...
    ;
  ;
|]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- instance IBounded (Bool)
bool_ibounded = [r|
  dIBoundedBool = Dict.IBounded (minimum,maximum) where
    minimum = func ->
      let dIBoundeda = ... in
        -- >> body
        Bool.False
        -- << body
      ;
    ;
    maximum = func ->
      let dIBoundedq = ... in
        Bool.True
      ;
    ;
  ;
|]

bool_ieq = [r|
  dIEqBool = Dict.IEq (eq_Bool) where
    eq_Bool = func ->
      let dIEqa = ... in
        func {dIEqa} ->
          let (x,y) = ... in
            or (and (x,y), (and (not x, not y)))
          ;
        ;
      ;
    ;
  ;
|]

bool_ienum = [r|
  dIEnumBool = Dict.IEq (toNat,fromNat) where
    toNat = func ->
      case ... of
        Bool.False -> Nat.Zero
        Bool.True  -> Nat.Succ Nat.Zero
      ;
    ;
    fromNat = func ->
      case ... of
        Nat.Zero          -> Bool.False
        Nat.Succ Nat.Zero -> Bool.True
      ;
    ;
  ;
|]

-- implementation IOrd for Bool
bool_iord = [r|
  -- dict
  dIOrdBool = Dict.IOrd (lt_Bool,lte_IOrd,gt_IOrd,gte_IOrd) where
    lt_Bool = func ->
      let dIEqa = ... in
        func {dIEqa} ->
          case ... of
            (Bool.False, Bool.False) -> Bool.False
            (Bool.False, Bool.True)  -> Bool.True
            (Bool.True,  Bool.False) -> Bool.False
            (Bool.True,  Bool.True)  -> Bool.False
          ;
        ;
      ;
    ;
  ;
|]

unit_ienum = [r|
  dIEnumUnit = Dict.IEq (toNat,fromNat) where
    toNat = func ->
      case ... of
        () -> Nat.Zero
      ;
    ;
    fromNat = func ->
      case ... of
        Nat.Zero -> ()
      ;
    ;
  ;
|]

-------------------------------------------------------------------------------

-- instance IEq (Int)
nat_ieq = [r|
  dIEqNat = Dict.IEq (eq_IEq)
|]
