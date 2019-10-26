module Dyn.ASTSpec (main,spec) where

import Test.Hspec

import Dyn.AST

main :: IO ()
main = hspec spec

spec = do

  describe "exprToString:" $ do
    it "a" $
      toString (EVar az "a") `shouldBe` "a"

  describe "declToString:" $ do
    it "a :: ()" $
      toString (DSig az "a" tz)
        `shouldBe` "a :: ()"
    it "a = b" $
      toString (DAtr az (PWrite az "a") (Where (az, EVar az "b", [])))
        `shouldBe` "a = b"
    it "a = b where\n  b=()" $
      toString
        (DAtr az (PWrite az "a")
          (Where (az, EVar az "b",
            [DSig az "b" tz])))
        `shouldBe` "a = b where\n  b :: ()\n;"

  describe "declToString:" $ do
    it "b where b=a, a=()" $
      toString (
        Where (az, EVar az "b", [
          DAtr az (PWrite az "b") (Where (az, EVar az "a",[])),
          DAtr az (PWrite az "a") (Where (az, EUnit az,[]))
        ]))
        `shouldBe` "b where\n  b = a\n  a = ()\n;"

  describe "progToString:" $ do
    it "v" $
      toString (Where (az, EVar az "v", [])) `shouldBe` "v"
