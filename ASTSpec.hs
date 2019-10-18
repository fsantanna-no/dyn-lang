module ASTSpec where

import Test.Hspec

import Dyn.AST

main :: IO ()
main = hspec $ do

  describe "exprToString:" $ do
    it "a" $
      exprToString (EVar az "a") `shouldBe` "a"

  describe "dclToString:" $ do
    it "a :: () = b" $
      dclToString 0
        (Dcl ("a", (),
          Where (EVar az "b", []))) `shouldBe` "a :: () = b"
    it "a :: () = b where\n  b=()" $
      dclToString 0
        (Dcl ("a", (),
          Where (EVar az "b",
            [Dcl ("b", (), Where (EUnit az,[]))])))
        `shouldBe` "a :: () = b where\n  b :: () = ()"

  describe "dclToString:" $ do
    it "b where b=a, a=()" $
      whereToString 0 (
        Where (EVar az "b", [
          Dcl ("b", (), Where (EVar az "a",[])),
          Dcl ("a", (), Where (EUnit az,[]))
        ]))
        `shouldBe` "b where\n  b :: () = a\n  a :: () = ()"

  describe "progToString:" $ do
    it "v" $
      progToString (Where (EVar az "v", [])) `shouldBe` "v"
