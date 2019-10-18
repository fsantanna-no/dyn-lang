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
      dclToString
        (Dcl ("a", (),
          Where (EVar az "b", []))) `shouldBe` "a :: () = b"
    it "a :: () = b where b=()" $
      dclToString
        (Dcl ("a", (),
          Where (EVar az "b",
            [Dcl ("b", (), Where (EUnit az,[]))])))
        `shouldBe` "a :: () = b where b :: () = ()"
    it "b where b=a, a=()" $
      whereToString (
        Where (EVar az "b", [
          Dcl ("b", (), Where (EVar az "a",[])),
          Dcl ("a", (), Where (EUnit az,[]))
        ]))
        `shouldBe` "b where b :: () = a,a :: () = ()"
