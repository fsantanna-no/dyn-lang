module Dyn.EvalSpec (main,spec) where

import Test.Hspec

import Dyn.AST
import Dyn.Eval

main :: IO ()
main = hspec spec

spec = do

  describe "evalExpr:" $ do
    it "()" $
      evalExpr [] (EUnit az) `shouldBe` (EUnit az)
    it "a" $
      evalExpr [("a",EUnit az)] (EVar az "a") `shouldBe` (EUnit az)

  describe "evalWhere:" $ do
    it "()" $
      evalWhere []
        (Where (az, EUnit az, []))
        `shouldBe` (EUnit az)
    it "b where b=a, a=()" $
      evalWhere []
        (Where (az, EVar az "b", [
          Dcl (az, "b", Just tz, Just $ Where (az, EVar az "a",[])),
          Dcl (az, "a", Just tz, Just $ Where (az, EUnit az,[]))
        ]))
        `shouldBe` (EUnit az)
    it "b where b::() b=()" $
      evalWhere []
        (Where (az, EVar az "b", [
          Dcl (az, "b", Just tz, Nothing),
          Dcl (az, "b", Nothing, Just $ Where (az, EUnit az,[]))
        ]))
        `shouldBe` (EUnit az)

  describe "evalDcl:" $ do
    it "a=()" $
      evalDcl [] (Dcl (az, "a", Just tz, Just $ Where (az, EUnit az,[])))
        `shouldBe` [("a", EUnit az)]
