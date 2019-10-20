{-# LANGUAGE QuasiQuotes #-}

module Dyn.ParserSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Text.Parsec.String (Parser)

import Dyn.Parser
import Dyn.AST

fromRight (Right x) = x

main :: IO ()
main = hspec spec

spec = do

  describe "tokens:" $ do
    it "-- xxx " $
      parse' tk_comm "-- xxx "
        `shouldBe` Right ()
    it "xxx" $
      parse' tk_var "xxx "
        `shouldBe` Right "xxx"
    it "A" $
      parse' tk_data "A"
        `shouldBe` Right "A"
    it "A.B" $
      parse' tk_hier "A.B"
        `shouldBe` Right ["A","B"]

  describe "expr_*:" $ do
    it "xxx" $
      parse' expr_var "xxx"
        `shouldBe` Right (EVar az{pos=(1,1)} "xxx")
    it "A" $
      parse' expr_cons "A"
        `shouldBe` Right (ECons az{pos=(1,1)} ["A"])
    it "A.B" $
      parse' expr_cons "A.B"
        `shouldBe` Right (ECons az{pos=(1,1)} ["A","B"])
  describe "expr:" $ do
    it "(())" $
      parse' expr "(())"
        `shouldBe` Right (EUnit az{pos=(1,2)})
    it "func" $
      parse' expr "func () ();"
        `shouldBe` Right (EFunc az{pos=(1,1)} tz (Where (az{pos=(1,9)}, EUnit az{pos=(1,9)},[])))
    it "a b c" $
      parse' expr "a b c"
        `shouldBe` Left "(line 1, column 5):\nunexpected 'c'\nexpecting end of input"
    it "a b " $
      parse' expr "a b "
        `shouldBe` Right (ECall az{pos=(1,1)} (EVar az{pos=(1,1)} "a") (EVar az{pos=(1,3)} "b"))
    it "error" $
      parse' expr "error"
        `shouldBe` Right (EError az{pos=(1,1)} "<user>")
    it "errors" $
      parse' expr "errors"
        `shouldBe` Right (EVar az{pos=(1,1)} "errors")
    it "arg" $
      parse' expr "..."
        `shouldBe` Right (EArg az{pos=(1,1)})

  describe "where:" $ do
    it "x" $
      parse' where_ "x"
        `shouldBe` Right (Where (az{pos=(1,1)}, EVar az{pos=(1,1)} "x", []))

  describe "dcl:" $ do
    it "x :: () = ()" $
      parse' dcl "x :: () = ()"
        `shouldBe` Right (Dcl (az{pos=(1,1)}, PWrite az{pos=(1,1)} "x", Just tz, Just $ Where (az{pos=(1,11)}, EUnit az{pos=(1,11)}, [])))
    it "x :: ()" $
      parse' dcl "x :: ()"
        `shouldBe` Right (Dcl (az{pos=(1,1)}, PWrite az{pos=(1,1)} "x", Just tz, Nothing))
    it "x = ()" $
      parse' dcl "x = ()"
        `shouldBe` Right (Dcl (az{pos=(1,1)}, PWrite az{pos=(1,1)} "x", Nothing, Just $ Where (az{pos=(1,5)},  EUnit az{pos=(1,5)},  [])))
    it "x" $
      parse' dcl "x"
        `shouldBe` Left "(line 1, column 2):\nunexpected end of input\nexpecting identifier, \"::\" or \"=\""

  describe "toString:" $ do
    describe "expr_*:" $ do
      it "()" $
        (exprToString 0 $ fromRight $ parse' expr_unit "()")
          `shouldBe` "()"
      it "xxx" $
        (exprToString 0 $ fromRight $ parse' expr_var "xxx")
          `shouldBe` "xxx"
      it "(xxx,yyy)" $
        (exprToString 0 $ fromRight $ parse' expr_tuple "(xxx, yyy)")
          `shouldBe` "(xxx,yyy)"
    describe "expr:" $ do
      it "()" $
        (exprToString 0 $ fromRight $ parse' expr "()")
          `shouldBe` "()"
      it "(())" $
        (exprToString 0 $ fromRight $ parse' expr "(())")
          `shouldBe` "()"
      it "A.B" $
        (exprToString 0 $ fromRight $ parse' expr "A.B")
          `shouldBe` "A.B"
      it "func" $
        (exprToString 0 $ fromRight $ parse' expr "func () xxx;")
          `shouldBe` "func ()\n  xxx"
      it "func" $
        (exprToString 0 $ fromRight $ parse' expr "func () xxx where xxx=() where y=();\n  x=();;")
          `shouldBe` "func ()\n  xxx where\n    xxx = () where\n      y = ()\n    x = ()"
      it "func" $
        (exprToString 0 $ fromRight $ parse' expr "func () xxx where\n  xxx=() where\n    y=()\n    x=();;;")
          `shouldBe` "func ()\n  xxx where\n    xxx = () where\n      y = ()\n      x = ()"
      it "call" $
        (exprToString 0 $ fromRight $ parse' expr "(a (b c)) d")
          `shouldBe` "((a (b c)) d)"
      it "if x ~ y then t else f" $
        (exprToString 0 $ fromRight $ parse' expr "if x ~ y then t else f;")
          `shouldBe` "if x ~ y then\n  t\nelse\n  f"
    describe "prog:" $ do
      it "x where x :: () = ()" $
        (progToString $ fromRight $ parse "main :: () = ()")
          `shouldBe` "main where\n  main :: () = ()"
      it "x where x :: ()" $
        (progToString $ fromRight $ parse "main :: ()")
          `shouldBe` "main where\n  main :: ()"
      it "x where x = ()" $
        (progToString $ fromRight $ parse "main = ()")
          `shouldBe` "main where\n  main = ()"
      it "x where x,y" $
        (parseToString "main::()=y  y::()=()")
          `shouldBe` "(line 1, column 14):\nunexpected ':'\nexpecting identifier, \"where\", pattern or end of input"
      it "x where x,y" $
        (parseToString "main::()=y\ny::()=()")
          `shouldBe` "main where\n  main :: () = y\n  y :: () = ()"
      it "where-newline" $
        (progToString $ fromRight $ parse "main :: () = f ()\n")
          `shouldBe` "main where\n  main :: () = (f ())"
      it "Xx a = ()" $
        (progToString $ fromRight $ parse "Xx main = ()")
          `shouldBe` "main where\n  (Xx main) = ()"
      it "func" $
        (progToString $ fromRight $ parse
          [r|
main :: () = f ()
f :: () = func () x where
            x :: () = ...;
          ;
|])
          `shouldBe` "main where\n  main :: () = (f ())\n  f :: () = func ()\n  x where\n    x :: () = ..."

      it "where-where" $
        (parseToString
          [r|
main :: () = b d where
  b :: () = c
  c :: () = ()
  ;
d :: () = ()
|])
          `shouldBe` "main where\n  main :: () = (b d) where\n    b :: () = c\n    c :: () = ()\n  d :: () = ()"

    describe "parseToString:" $ do

      it "case" $
        parseToString "main = case x of Bool.True -> a\nBool.False -> b;"
          `shouldBe` "xxx"
      it "case" $
        parseToString "main = case x of _ -> a;"
          `shouldBe` "xxx"

      it "Nat +" $
        parseToString [r|
main = add (Nat.Zero, Nat.Succ Nat.Zero)
add =
  func ()
    if y ~ Nat.Zero then
      x
    else
      Nat.Succ (add (x,z)) where
        Nat.Succ z = y
      ;
    ; where
      (x,y) = ...
    ;
  ;
|]
        `shouldBe` "main where\n  main = (add (Nat.Zero,(Nat.Succ Nat.Zero)))\n  add = func ()\n  if y ~ Nat.Zero then\n  x\nelse\n  (Nat.Succ (add (x,z))) where\n    (Nat.Succ z) = y where\n    (x,y) = ..."
