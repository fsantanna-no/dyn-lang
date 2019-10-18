module ASTSpec where

import Test.Hspec

import Dyn.AST

main :: IO ()
main = hspec $ do
  describe "exprToString" $ do
    it "a" $
      exprToString 0 (EVar az "a") `shouldBe` "a"
