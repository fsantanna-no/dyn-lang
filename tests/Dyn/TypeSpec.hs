{-# LANGUAGE QuasiQuotes #-}

module Dyn.TypeSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Type

main :: IO ()
main = hspec spec

caieq  = [("a",["IEq"])]

spec = do

  describe "getType:" $ do
    it "()" $
      getType [] cz (EUnit az) `shouldBe` Type (az, TUnit, cz)
    it "x" $
      getType [DSig az "x" (Type (az,TUnit,cz))] cz (EVar az "x")
        `shouldBe` Type (az, TUnit, cz)
    it "x :: a" $
      getType [DSig az "x" (Type (az,TVar "a",caieq))] caieq (EVar az "x")
        `shouldBe` Type (az, TVar "a", caieq)
    it "x :: a" $
      getType [DSig az "x" (Type (az,TVar "a",caieq))] cz (EVar az "x")
        `shouldBe` Type (az, TVar "a", caieq)
{-
    it "[] x" $
      poly [DSig az "x" (Type (az,TUnit,cz))] cz (EVar az "x")
        `shouldBe` False
    it "[a] x" $
      poly [DSig az "x" (Type (az,TUnit,cz))] caieq (EVar az "x")
        `shouldBe` False
    it "x :: a" $
      poly [DSig az "x" (Type (az,TVar "a",caieq))] cz (EVar az "x")
        `shouldBe` True
    it "[a] x :: a" $
      poly [DSig az "x" (Type (az,TVar "a",caieq))] caieq (EVar az "x")
        `shouldBe` False
-}
