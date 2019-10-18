{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import Test.Hspec

import qualified Text.Parsec as P (eof, parse)
import Text.Parsec.String (Parser)

import Dyn.Parser
import Dyn.AST

fromRight (Right x) = x

parse :: Parser a -> String -> Either String a
parse rule input =
  case P.parse (rule <* P.eof) "" input of
    (Right v) -> Right v
    (Left  v) -> Left (show v)

main :: IO ()
main = hspec $ do

  describe "tokens:" $ do
    it "-- xxx " $
      parse tk_comm "-- xxx "
        `shouldBe` Right ()
    it "xxx" $
      parse tk_var "xxx "
        `shouldBe` Right "xxx"
    it "A" $
      parse tk_data "A"
        `shouldBe` Right "A"
    it "A.B" $
      parse tk_hier "A.B"
        `shouldBe` Right ["A","B"]

  describe "expr_*:" $ do
    it "xxx" $
      parse expr_var "xxx"
        `shouldBe` Right (EVar az{pos=(1,1)} "xxx")
    it "A.B" $
      parse expr_cons "A.B"
        `shouldBe` Right (ECons az{pos=(1,1)} ["A","B"])
  describe "expr:" $ do
    it "(())" $
      parse expr "(())"
        `shouldBe` Right (EUnit az{pos=(1,2)})
    it "func" $
      parse expr "func () ()"
        `shouldBe` Right (EFunc az{pos=(1,1)} () (EUnit az{pos=(1,9)}))
    it "a b c" $
      parse expr "a b c"
        `shouldBe` Left "(line 1, column 5):\nunexpected 'c'\nexpecting end of input"
    it "a b " $
      parse expr "a b "
        `shouldBe` Right (ECall az{pos=(1,1)} (EVar az{pos=(1,1)} "a") (EVar az{pos=(1,3)} "b"))
    it "error" $
      parse expr "error"
        `shouldBe` Right (EError az{pos=(1,1)})
    it "errors" $
      parse expr "errors"
        `shouldBe` Right (EVar az{pos=(1,1)} "errors")
    it "arg" $
      parse expr "..."
        `shouldBe` Right (EArg az{pos=(1,1)})

  describe "toString:" $ do
    describe "expr_*:" $ do
      it "()" $
        (exprToString $ fromRight $ parse expr_unit "()")
          `shouldBe` "()"
      it "xxx" $
        (exprToString $ fromRight $ parse expr_var "xxx")
          `shouldBe` "xxx"
      it "(xxx,yyy)" $
        (exprToString $ fromRight $ parse expr_tuple "(xxx, yyy)")
          `shouldBe` "(xxx,yyy)"
    describe "expr:" $ do
      it "()" $
        (exprToString $ fromRight $ parse expr "()")
          `shouldBe` "()"
      it "(())" $
        (exprToString $ fromRight $ parse expr "(())")
          `shouldBe` "()"
      it "A.B" $
        (exprToString $ fromRight $ parse expr "A.B")
          `shouldBe` "A.B"
      it "func" $
        (exprToString $ fromRight $ parse expr "func () xxx")
          `shouldBe` "func () xxx"
      it "call" $
        (exprToString $ fromRight $ parse expr "(a (b c)) d")
          `shouldBe` "((a (b c)) d)"
      it "if x matches y then t else f" $
        (exprToString $ fromRight $ parse expr "if x matches y then t else f")
          `shouldBe` "if x matches y then t else f"
