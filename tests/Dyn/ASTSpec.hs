module Dyn.ASTSpec (main,spec) where

import Test.Hspec

import Dyn.AST
import Dyn.Classes

main :: IO ()
main = hspec spec

spec = do

  describe "exprToString:" $ do
    it "a" $
      toString (EVar pz "a") `shouldBe` "a"

  describe "declToString:" $ do
    it "a :: ?" $
      toString (DSig pz "a" tz)
        `shouldBe` "a :: ?"
    it "a = b" $
      toString (DAtr pz (PWrite pz "a") (ExpWhere (pz, EVar pz "b", [])))
        `shouldBe` "a = b"
    it "a = b where\n  b=()" $
      toString
        (DAtr pz (PWrite pz "a")
          (ExpWhere (pz, EVar pz "b",
            [DSig pz "b" tz])))
        `shouldBe` "a = b where\n  b :: ?\n;"

  describe "declToString:" $ do
    it "b where b=a, a=()" $
      toString (
        ExpWhere (pz, EVar pz "b", [
          DAtr pz (PWrite pz "b") (ExpWhere (pz, EVar pz "a",[])),
          DAtr pz (PWrite pz "a") (ExpWhere (pz, EUnit pz,[]))
        ]))
        `shouldBe` "b where\n  b = a\n  a = ()\n;"

  describe "progToString:" $ do
    it "v" $
      toString (ExpWhere (pz, EVar pz "v", [])) `shouldBe` "v"
