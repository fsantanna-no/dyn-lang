{-# LANGUAGE QuasiQuotes #-}

module Dyn.IFaceSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Parser
import Dyn.Eval

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

main :: IO ()
main = hspec spec

spec = do

  describe "manual" $ do

    it "XXX: IEqualable: default neq" $
      run ([r|
main = v where  -- neq (eq(T,T), F)
  -- typesystem renames to concrete type methods
  v = neq_bool (eq_bool (Bool.True,Bool.True), Bool.False)
;

-- instance IEq (Bool)

-- Wrappers are closures with fied "ieq_bool" dict
  eq_bool  = func -> eq (ieq_bool,x,y) where
              (x,y)  = ...
              (eq,_) = ieq_bool
             ;;
  neq_bool = func -> neq (ieq_bool,x,y) where
              (x,y)   = ...
              (_,neq) = ieq_bool
             ;;

-- Dict receives eq/neq methods.
--  - implements eq, uses default neq
--  - methods receive extra dict
  ieq_bool = (eq_bool_,neq_) where
    eq_bool_ = func ->
      or (and (x,y), (and (not x, not y))) where
        (_,x,y) = ...
      ;
    ;
  ;

-- interface IEq(eq,neq)
-- Methods are renamed to include "dict" param:
--  - eq_ is not implemented
--  - neq_ has a default implentation
  neq_ = func ->
    not (eq (...)) where
      ((eq,_),_,_) = ...
    ;
  ;

|] ++ bool)
        `shouldBe` "Bool.True"

