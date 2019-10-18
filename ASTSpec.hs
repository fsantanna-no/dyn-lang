module ASTSpec where

import Test.Hspec

import Dyn.AST

main :: IO ()
main = hspec $ do
  describe "exprToString:" $ do
    it "a" $
      exprToString (EVar az "a") `shouldBe` "a"
