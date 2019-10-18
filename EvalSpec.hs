module EvalSpec where

import Test.Hspec

import Dyn.AST
import Dyn.Eval

main :: IO ()
main = hspec $ do

  describe "evalExpr:" $ do
    it "()" $
      evalExpr [] (EUnit az) `shouldBe` (EUnit az)

  describe "evalWhere:" $ do
    it "()" $
      evalWhere
        (Where (az, EUnit az, []))
        `shouldBe` (EUnit az)
    it "b where b=a, a=()" $
      evalWhere
        (Where (az, EVar az "b", [
          Dcl (az, "b", (), Where (az, EVar az "a",[])),
          Dcl (az, "a", (), Where (az, EUnit az,[]))
        ]))
        `shouldBe` (EUnit az)

  describe "evalDcl:" $ do
    it "a=()" $
      evalDcl (Dcl (az, "a", (), Where (az, EUnit az,[])))
        `shouldBe` ("a", EUnit az)
