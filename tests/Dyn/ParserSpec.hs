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
      parse' expr "func :: () -> ();"
        `shouldBe` Right (EFunc az{pos=(1,1)} tz [] (Where (az{pos=(1,15)}, EUnit az{pos=(1,15)},[])))
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
    describe "dcl:" $ do
      it "case" $
        (dclToString 0 $ fromRight $ parse' dcl "main = case x of Bool.True -> a\nBool.False -> b;")
          `shouldBe` "main = case x of\n  Bool.True -> a\n  Bool.False -> b\n;"
    describe "where:" $ do
      it "case" $
        (whereToString 0 $ fromRight $ parse' where_ "case x of Bool.True -> a\nBool.False -> b;")
          `shouldBe` "case x of\n  Bool.True -> a\n  Bool.False -> b\n;"
    describe "prog:" $ do
      it "case" $
        (whereToString 0 $ fromRight $ parse' (prog) "main = case x of Bool.True -> a\nBool.False -> b;")
          `shouldBe` "main where\n  main = case x of\n    Bool.True -> a\n    Bool.False -> b\n  ;\n;"

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
        (exprToString 0 $ fromRight $ parse' expr "func :: () -> xxx;")
          `shouldBe` "func :: () ->\n  xxx\n;"
      it "func" $
        (exprToString 0 $ fromRight $ parse' expr "func :: () -> xxx where xxx=() where y=();\n  x=();;")
          `shouldBe` "func :: () ->\n  xxx where\n    xxx = () where\n      y = ()\n    ;\n    x = ()\n  ;\n;"
      it "func" $
        (exprToString 0 $ fromRight $ parse' expr "func -> xxx where\n  xxx=() where\n    y=()\n    x=();;;")
          `shouldBe` "func :: () ->\n  xxx where\n    xxx = () where\n      y = ()\n      x = ()\n    ;\n  ;\n;"
      it "call" $
        (exprToString 0 $ fromRight $ parse' expr "(a (b c)) d")
          `shouldBe` "((a (b c)) d)"
      it "case x of ~y->t\n_->f" $
        (exprToString 0 $ fromRight $ parse' expr "case x of ~y->t \n _->f;")
          `shouldBe` "case x of\n  y -> t\n  _ -> f\n;"
    describe "prog:" $ do
      it "x where x :: () = ()" $
        (progToString $ fromRight $ parse "main :: () = ()")
          `shouldBe` "main where\n  main :: () = ()\n;"
      it "x where x :: ()" $
        (progToString $ fromRight $ parse "main :: ()")
          `shouldBe` "main where\n  main :: ()\n;"
      it "x where x = ()" $
        (progToString $ fromRight $ parse "main = ()")
          `shouldBe` "main where\n  main = ()\n;"
      it "x where x,y" $
        (parseToString "main::()=y  y::()=()")
          `shouldBe` "(line 1, column 14):\nunexpected ':'\nexpecting identifier, \"where\", pattern or end of input"
      it "x where x,y" $
        (parseToString "main::()=y\ny::()=()")
          `shouldBe` "main where\n  main :: () = y\n  y :: () = ()\n;"
      it "where-newline" $
        (progToString $ fromRight $ parse "main :: () = f ()\n")
          `shouldBe` "main where\n  main :: () = (f ())\n;"
      it "Xx a = ()" $
        (progToString $ fromRight $ parse "Xx main = ()")
          `shouldBe` "main where\n  (Xx main) = ()\n;"
      it "func" $
        (progToString $ fromRight $ parse
          [r|
main :: () = f ()
f :: () = func -> x where
            x :: () = ...;
          ;
|])
          `shouldBe` "main where\n  main :: () = (f ())\n  f :: () = func :: () ->\n    x where\n      x :: () = ...\n    ;\n  ;\n;"

      it "where-where" $
        (parseToString
          [r|
main :: () = b d where
  b :: () = c
  c :: () = ()
  ;
d :: () = ()
|])
          `shouldBe` "main where\n  main :: () = (b d) where\n    b :: () = c\n    c :: () = ()\n  ;\n  d :: () = ()\n;"

    describe "parseToString:" $ do

      it "case" $
        parseToString "main = case x of Bool.True -> a\nBool.False -> b;"
          `shouldBe` "main where\n  main = case x of\n    Bool.True -> a\n    Bool.False -> b\n  ;\n;"
      it "case" $
        parseToString "main = case x of _ -> a;"
          `shouldBe` "main where\n  main = case x of\n    _ -> a\n  ;\n;"

      it "Nat +" $
        parseToString [r|
main = add (Nat.Zero, Nat.Succ Nat.Zero)
add =
  func :: () ->
    case y of
      Nat.Zero    -> x
      Nat.Succ =z -> Nat.Succ (add (x,z))
    ; where
      (x,y) = ...
    ;
  ;
|]
        `shouldBe` "main where\n  main = (add (Nat.Zero,(Nat.Succ Nat.Zero)))\n  add = func :: () ->\n    case y of\n      Nat.Zero -> x\n      (Nat.Succ z) -> (Nat.Succ (add (x,z)))\n    ; where\n      (x,y) = ...\n    ;\n  ;\n;"
