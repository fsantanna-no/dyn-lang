module EvalSpec where

import Test.Hspec

import Dyn.AST
import Dyn.Eval

main :: IO ()
main = hspec $ do

  describe "evalExpr:" $ do
    it "()" $
      evalExpr (EUnit az) `shouldBe` (EUnit az)
