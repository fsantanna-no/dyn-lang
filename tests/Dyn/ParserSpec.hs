{-# LANGUAGE QuasiQuotes #-}

module Dyn.ParserSpec (main,spec) where

import Debug.Trace
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
        `shouldBe` Right (EVar (1,1) "xxx")
    it "A" $
      parse' expr_cons "A"
        `shouldBe` Right (ECons (1,1) ["A"])
    it "A.B" $
      parse' expr_cons "A.B"
        `shouldBe` Right (ECons (1,1) ["A","B"])
  describe "expr:" $ do
    it "(())" $
      parse' expr "(())"
        `shouldBe` Right (EUnit (1,2))
    it "func" $
      parse' expr "func :: () -> ();"
        `shouldBe` Right (EFunc (1,1) (Type ((1,9), TUnit, [])) [] (Where ((1,15), EUnit (1,15),[])))
    it "a b c" $
      parse' expr "a b c"
        `shouldBe` Left "(line 1, column 5):\nunexpected 'c'\nexpecting end of input"
    it "a b " $
      parse' expr "a b "
        `shouldBe` Right (ECall (1,1) (EVar (1,1) "a") (EVar (1,3) "b"))
    it "error" $
      parse' expr "error"
        `shouldBe` Right (EError (1,1) "<user>")
    it "errors" $
      parse' expr "errors"
        `shouldBe` Right (EVar (1,1) "errors")
    it "arg" $
      parse' expr "..."
        `shouldBe` Right (EArg (1,1))

  describe "where:" $ do
    it "x" $
      parse' where_ "x"
        `shouldBe` Right (Where ((1,1), EVar (1,1) "x", []))

  describe "decl:" $ do
    it "x :: () = ()" $
      parse' decl "x :: () = ()"
        `shouldBe` Right [DSig (1,1) "x" (Type ((1,6), TUnit, [])),DAtr (1,1) (PWrite (1,1) "x") (Where ((1,11), EUnit (1,11), []))]
    it "x :: ()" $
      parse' decl "x :: ()"
        `shouldBe` Right [DSig (1,1) "x" (Type ((1,6), TUnit, []))]
    it "x = ()" $
      parse' decl "x = ()"
        `shouldBe` Right [DAtr (1,1) (PWrite (1,1) "x") (Where ((1,5),  EUnit (1,5),  []))]
    it "x" $
      parse' decl "x"
        `shouldBe` Left "(line 1, column 2):\nunexpected end of input\nexpecting identifier or \"=\""

  describe "toString:" $ do
    describe "expr_*:" $ do
      it "()" $
        (toString $ fromRight $ parse' expr_unit "()")
          `shouldBe` "()"
      it "xxx" $
        (toString $ fromRight $ parse' expr_var "xxx")
          `shouldBe` "xxx"
      it "(xxx,yyy)" $
        (toString $ fromRight $ parse' expr_tuple "(xxx, yyy)")
          `shouldBe` "(xxx,yyy)"
    describe "decl:" $ do
      it "case" $
        (toString $ head $ fromRight $ parse' decl "main = case x of Bool.True -> a\nBool.False -> b;")
          `shouldBe` "main = case x of\n  Bool.True -> a\n  Bool.False -> b\n;"
    describe "where:" $ do
      it "case" $
        (toString $ fromRight $ parse' where_ "case x of Bool.True -> a\nBool.False -> b;case")
          `shouldBe` "case x of\n  Bool.True -> a\n  Bool.False -> b\n;"
    describe "prog:" $ do
      it "case" $
        (toString $ fromRight $ parse' (prog) "main = case x of Bool.True -> a\nBool.False -> b;")
          `shouldBe` "main = case x of\n  Bool.True -> a\n  Bool.False -> b\n;\n"

    describe "expr:" $ do
      it "()" $
        (toString $ fromRight $ parse' expr "()")
          `shouldBe` "()"
      it "(())" $
        (toString $ fromRight $ parse' expr "(())")
          `shouldBe` "()"
      it "A.B" $
        (toString $ fromRight $ parse' expr "A.B")
          `shouldBe` "A.B"
      it "func" $
        (toString $ fromRight $ parse' expr "func :: () -> xxx;func")
          `shouldBe` "func :: () ->\n  xxx\n;"
      it "func" $
        (toString $ fromRight $ parse' expr "func :: () -> xxx where xxx=() where y=();\n  x=();;")
          `shouldBe` "func :: () ->\n  xxx where\n    xxx = () where\n      y = ()\n    ;\n    x = ()\n  ;\n;"
      it "func" $
        (toString $ fromRight $ parse' expr "func -> xxx where\n  xxx=() where\n    y=()\n    x=();where;where;func")
          `shouldBe` "func :: () ->\n  xxx where\n    xxx = () where\n      y = ()\n      x = ()\n    ;\n  ;\n;"
      it "call" $
        (toString $ fromRight $ parse' expr "(a (b c)) d")
          `shouldBe` "((a (b c)) d)"
      it "case x of ~y->t\n_->f" $
        (toString $ fromRight $ parse' expr "case x of ~y->t \n _->f;")
          `shouldBe` "case x of\n  ~y -> t\n  _ -> f\n;"
    describe "prog:" $ do
      it "x where x :: () = ()" $
        (toString $ fromRight $ parse "main :: () = ()")
          `shouldBe` "main :: ()\nmain = ()\n"
      it "x where x :: ()" $
        (toString $ fromRight $ parse "main :: ()")
          `shouldBe` "main :: ()\n"
      it "x where x = ()" $
        (toString $ fromRight $ parse "main = ()")
          `shouldBe` "main = ()\n"
      it "x where x,y" $
        (parseToString False "main::()=y  y::()=()")
          `shouldBe` "(line 1, column 14):\nunexpected ':'\nexpecting identifier, \"where\", declaration, \"interface\", \"implementation\" or end of input"
      it "x where x,y" $
        (parseToString False "main::()=y\ny::()=()")
          `shouldBe` "main :: ()\nmain = y\ny :: ()\ny = ()\n"
      it "where-newline" $
        (toString $ fromRight $ parse "main :: () = f ()\n")
          `shouldBe` "main :: ()\nmain = (f ())\n"
      it "Xx a = ()" $
        (toString $ fromRight $ parse "Xx main = ()")
          `shouldBe` "(Xx main) = ()\n"
      it "func" $
        (toString $ fromRight $ parse
          [r|
main :: () = f ()
f :: () = func -> x where
            x :: () = ...;
          ;
|])
          `shouldBe` "main :: ()\nmain = (f ())\nf :: ()\nf = func :: () ->\n  x where\n    x :: ()\n    x = ...\n  ;\n;\n"

      it "where-where" $
        (parseToString False
          [r|
main :: () = b d where
  b :: () = c
  c :: () = ()
  ;
d :: () = ()
|])
          `shouldBe` "main :: ()\nmain = (b d) where\n  b :: ()\n  b = c\n  c :: ()\n  c = ()\n;\nd :: ()\nd = ()\n"

    describe "parseToString:" $ do

      it "case" $
        parseToString False "main = case x of Bool.True -> a\nBool.False -> b;"
          `shouldBe` "main = case x of\n  Bool.True -> a\n  Bool.False -> b\n;\n"
      it "case" $
        parseToString False "main = case x of _ -> a;"
          `shouldBe` "main = case x of\n  _ -> a\n;\n"

      it "Nat +" $
        parseToString False [r|
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
        `shouldBe` "main = (add (Nat.Zero,(Nat.Succ Nat.Zero)))\nadd = func :: () ->\n  case y of\n    Nat.Zero -> x\n    (Nat.Succ =z) -> (Nat.Succ (add (x,z)))\n  ; where\n    (x,y) = ...\n  ;\n;\n"
