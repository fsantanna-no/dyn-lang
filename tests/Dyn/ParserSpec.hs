{-# LANGUAGE QuasiQuotes #-}

module Dyn.ParserSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

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
main = hspec spec

spec = do

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

  describe "list:" $ do
    it "both: (),()" $
      parse (list_both (tk_sym ",") (tk_sym "()")) "(),()"
        `shouldBe` Right [(),()]
    it "list: ((),())" $
      parse (list (tk_sym "()")) "((),())"
        `shouldBe` Right [(),()]
    it "list: ()\n()" $
      parse (list (tk_sym "()")) "()\n();"
        `shouldBe` Right [(),()]
    it "list" $
      parse (list expr) "(xxx, yyy)"
        `shouldBe` Right [EVar az{pos = (1,2)} "xxx",EVar az{pos = (1,7)} "yyy"]

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
        `shouldBe` Right (EFunc az{pos=(1,1)} tz (EUnit az{pos=(1,9)}))
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

  describe "where:" $ do
    it "x" $
      parse where_ "x"
        `shouldBe` Right (Where (az{pos=(1,1)}, EVar az{pos=(1,1)} "x", []))

  describe "dcl:" $ do
    it "x :: () = ()" $
      parse dcl "x :: () = ()"
        `shouldBe` Right (Dcl (az{pos=(1,1)}, "x", Just tz, Just $ Where (az{pos=(1,11)}, EUnit az{pos=(1,11)}, [])))
    it "x :: ()" $
      parse dcl "x :: ()"
        `shouldBe` Right (Dcl (az{pos=(1,1)}, "x", Just tz, Nothing))
    it "x = ()" $
      parse dcl "x = ()"
        `shouldBe` Right (Dcl (az{pos=(1,1)}, "x", Nothing, Just $ Where (az{pos=(1,5)},  EUnit az{pos=(1,5)},  [])))
    it "x" $
      parse dcl "x"
        `shouldBe` Left "(line 1, column 2):\nunexpected end of input\nexpecting identifier, \"::\" or \"=\""

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
      it "if x ~> y then t else f" $
        (exprToString $ fromRight $ parse expr "if x ~> y then t else f")
          `shouldBe` "if x ~> y then t else f"
    describe "prog:" $ do
      it "x where x :: () = ()" $
        (progToString $ fromRight $ parse prog "x where x :: () = ();")
          `shouldBe` "x where\n  x :: () = ()"
      it "x where x :: ()" $
        (progToString $ fromRight $ parse prog "x where x :: ();")
          `shouldBe` "x where\n  x :: ()"
      it "x where x = ()" $
        (progToString $ fromRight $ parse prog "x where x = ();")
          `shouldBe` "x where\n  x = ()"
      it "x where x,y" $
        (progToString $ fromRight $ parse prog "x where (x::()=y, y::()=())")
          `shouldBe` "x where\n  x :: () = y\n  y :: () = ()"
      it "func" $
        (progToString $ fromRight $ parse prog
          [r|
v where
  v :: () = f ()
  f :: () = func () x where
              x :: () = ...
            ;
  ;
|])
          `shouldBe` "v where\n  v :: () = (f ())\n  f :: () = func () x where\n    x :: () = ..."
      it "where-where" $
        (progToString $ fromRight $ parse prog
          [r|
a where
  a :: () = b d where
    b :: () = c
    c :: () = ()
    ;
  d :: () = ()
  ;
|])
          `shouldBe` "a where\n  a :: () = (b d) where\n    b :: () = c\n    c :: () = ()\n  d :: () = ()"
