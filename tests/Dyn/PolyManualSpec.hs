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

    it "implementation of IEq for a where a is IXxx" $
      evalString ([r|
main = (eq' (dIEqIXxx dIXxxXxx)) (Xxx,Xxx)

dIEqIXxx = Dict.IEq (eq,neq_IEq) where
  eq = func {f} ->  -- :: (ieq_xxx,a,a) -> Bool where a is IXxx
    eq (Dict.IEq (eq,neq), f ((f),x), f ((f),y)) where
      Dict.IEq (eq,neq) = dIEqBool
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

    it "XXX: f = func :: ((a -> Int) where a is IEq) -> eq (x,x)" $
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

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

prelude = nat_ieq ++ bool_iord ++ bool_ieq ++ iord ++ ieq ++ nat ++ bool

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

-- interface IEq(eq,neq)
ieq = [r|
  eq' = func ->
    eq ... where
      Dict.IEq (eq,_) = ...
    ;
  ;
  neq' = func ->
    neq ... where
      Dict.IEq (_,neq) = ...
    ;
  ;

  dIEq = Dict.IEq (eq_IEq,neq_IEq)

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

-- instance IEq (Bool)
bool_ieq = [r|
  -- Dict receives eq/neq methods.
  --  - implements eq, uses default neq
  --  - methods receive extra dict
  -- overrides default eq
  dIEqBool = Dict.IEq (eq_Bool,neq_IEq) where
    eq_Bool = func ->
      let dIEqa = ... in
        -- >> body
        func {dIEqa} ->
          let (x,y) = ... in
            or (and (x,y), (and (not x, not y)))
          ;
        ;
        -- << body
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

-------------------------------------------------------------------------------

-- instance IEq (Int)
nat_ieq = [r|
  dIEqNat = Dict.IEq (eq_IEq,neq_IEq)
|]
