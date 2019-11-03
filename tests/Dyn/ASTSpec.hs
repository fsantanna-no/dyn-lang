module Dyn.ASTSpec (main,spec) where

import Test.Hspec

import Dyn.AST
import Dyn.Classes

main :: IO ()
main = hspec spec

spec = do

  describe "exprToString:" $ do
    it "x" $
      toString (EVar pz "x") `shouldBe` "x"

  describe "declToString:" $ do
    it "x :: ?" $
      toString (DSig pz "x" cz TAny)
        `shouldBe` "x :: ?"
    it "x :: a where a is IEq" $
      toString (DSig pz "x" (Ctrs ["IEq"]) (TVar "a"))
        `shouldBe` "x :: a" -- where (a is IEq)"
    it "x = b" $
      toString (DAtr pz (PWrite pz "x") (ExpWhere (pz, EVar pz "b", [])))
        `shouldBe` "x = b"
    it "x = b where\n  b=()" $
      toString
        (DAtr pz (PWrite pz "x")
          (ExpWhere (pz, EVar pz "b",
            [DSig pz "b" cz TAny])))
        `shouldBe` "x = b where\n  b :: ?\n;"

  describe "declToString:" $ do
    it "b where b=x, x=()" $
      toString (
        ExpWhere (pz, EVar pz "b", [
          DAtr pz (PWrite pz "b") (ExpWhere (pz, EVar pz "x",[])),
          DAtr pz (PWrite pz "x") (ExpWhere (pz, EUnit pz,[]))
        ]))
        `shouldBe` "b where\n  b = x\n  x = ()\n;"

  describe "progToString:" $ do
    it "v" $
      toString (ExpWhere (pz, EVar pz "v", [])) `shouldBe` "v"
