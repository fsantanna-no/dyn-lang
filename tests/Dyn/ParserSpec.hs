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
        `shouldBe` Right (EFunc (1,1) (Ctrs []) TUnit [] (ExpWhere ((1,15),[], EUnit (1,15))))
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
        `shouldBe` Right (ExpWhere ((1,1), [], EVar (1,1) "x"))
    it "x" $
      parse' where_let "x"
        `shouldBe` Right (ExpWhere ((1,1), [], EVar (1,1) "x"))
    it "let x=() in x" $
      parse' where_let "let x=() in x;"
        `shouldBe` Right (ExpWhere ((1,1),[DAtr (1,5) (PWrite (1,5) "x") (ExpWhere ((1,7),[],EUnit (1,7)))],EVar (1,13) "x"))
    it "let x=() in y where y=x" $
      parse' where_let "let x=() in y where y=x;;"
        `shouldBe` Right (ExpWhere ((1,1),[DAtr (1,21) (PWrite (1,21) "y") (ExpWhere ((1,23),[],EVar (1,23) "x")),DAtr (1,5) (PWrite (1,5) "x") (ExpWhere ((1,7),[],EUnit (1,7)))],EVar (1,13) "y"))

  describe "decl:" $ do
    it "x :: () = ()" $
      parse' decl "x :: () = ()"
        `shouldBe` Right [DSig (1,1) "x" cz TUnit,DAtr (1,1) (PWrite (1,1) "x") (ExpWhere ((1,11), [], EUnit (1,11)))]
    it "x :: ()" $
      parse' decl "x :: ()"
        `shouldBe` Right [DSig (1,1) "x" cz TUnit]
    it "x = ()" $
      parse' decl "x = ()"
        `shouldBe` Right [DAtr (1,1) (PWrite (1,1) "x") (ExpWhere ((1,5),  [],  EUnit (1,5)))]
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
          `shouldBe` "func :: ? ->\n  xxx where\n    xxx = () where\n      y = ()\n      x = ()\n    ;\n  ;\n;"
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
        (parseToString "main::()=y  y::()=()")
          `shouldBe` "(line 1, column 14):\nunexpected ':'\nexpecting identifier, \"where\", declaration, \"data\", \"interface\", \"implementation\" or end of input"
      it "x where x,y" $
        (parseToString "main::()=y\ny::()=()")
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
          `shouldBe` "main :: ()\nmain = (f ())\nf :: ()\nf = func :: ? ->\n  x where\n    x :: ()\n    x = ...\n  ;\n;\n"

      it "where-where" $
        (parseToString
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
        parseToString "main = case x of Bool.True -> a\nBool.False -> b;"
          `shouldBe` "main = case x of\n  Bool.True -> a\n  Bool.False -> b\n;\n"
      it "case" $
        parseToString "main = case x of _ -> a;"
          `shouldBe` "main = case x of\n  _ -> a\n;\n"

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
        `shouldBe` "main = (add (Nat.Zero,(Nat.Succ Nat.Zero)))\nadd = func :: () ->\n  case y of\n    Nat.Zero -> x\n    (Nat.Succ =z) -> (Nat.Succ (add (x,z)))\n  ; where\n    (x,y) = ...\n  ;\n;\n"

-------------------------------------------------------------------------------

  describe "data" $ do

      it "data Xxx" $
        parseToString "data Xxx"
          `shouldBe` "data Xxx\n"
      it "data Xxx ; x = Xxx" $
        parseToString "data Xxx\nx::Xxx = Xxx"
          `shouldBe` "data Xxx\nx :: Xxx\nx = Xxx\n"

{-
      it "data Xxx.Yyy" $
        (parse stmt "data Xxx.Yyy")
        `shouldBe` Right (Data annz{source=("",1,1)} (TypeD ["Xxx","Yyy"] [] Type0,cz) False)
      it "data Xxx.Yyy" $
        (parse stmt "data Xxx.Yyy")
        `shouldBe` Right (Data annz{source=("",1,1)} (TypeD ["Xxx","Yyy"] [] Type0,cz) False)

      it "data Xxx with ()" $
        (parse stmt "data Xxx with ()")
        `shouldBe` Right (Data annz{source=("",1,1)} (TypeD ["Xxx"] [] Type0,cz) False)

      it "data Xxx with (Int,Int)" $
        (parse stmt "data Xxx with (Int,Int)")
        `shouldBe` Right (Data annz{source=("",1,1)} (TypeD ["Xxx"] [] (TypeN [int,int]),cz) False)

      it "data Xxx with (Int)" $
        (parse' stmt "data Xxx with (Int)")
        `shouldBe` Right (Data annz (TypeD ["Xxx"] [] int,cz) False)

      it "data Xxx with Int ; x<-Xxx(1,1)" $
        (parse' stmt "data Xxx with Int ; var x:Xxx <- Xxx 1")
        `shouldBe` Right (Seq annz (Data annz (TypeD ["Xxx"] [] int,cz) False) (Seq annz (Seq annz (Var annz "x" (TypeD ["Xxx"] [] Type0,cz)) (Nop annz)) (Set annz False (LVar "x") (Cons annz ["Xxx"] (Number annz 1)))))

      it "TODO-fields: data Xxx (x,y) with (Int,Int)" $
        (parse stmt "data Xxx (x,y) with (Int,Int)")
        `shouldBe` Left "TODO-fields"

      it "Xxx.Yyy" $
        (parse' stmt "data Int ; data Xxx with Int ; data Xxx.Yyy with Int ; var y:Xxx.Yyy <- Xxx.Yyy (1,2)")
        `shouldBe` Right (Seq annz (Data annz (TypeD ["Int"] [] Type0,M.fromList []) False) (Seq annz (Data annz (TypeD ["Xxx"] [] int,M.fromList []) False) (Seq annz (Data annz (TypeD ["Xxx","Yyy"] [] int,M.fromList []) False) (Seq annz (Seq annz (Var annz "y" (TypeD ["Xxx","Yyy"] [] Type0,M.fromList [])) (Nop annz)) (Set annz False (LVar "y") (Cons annz ["Xxx","Yyy"] (Tuple annz [Number annz 1,Number annz 2])))))))

      it "data X with Int ; x:Int ; X x <- X 1 ; ret x" $
        (parse' stmt "data Xxx with Int ; var x:Int ; set Xxx x <- Xxx 1 ; return x")
        `shouldBe` Right (Seq annz (Data annz (TypeD ["Xxx"] [] int,cz) False) (Seq annz (Seq annz (Seq annz (Var annz "x" (int,cz)) (Nop annz)) (Nop annz)) (Seq annz (Set annz False (LCons ["Xxx"] (LVar "x")) (Cons annz ["Xxx"] (Number annz 1))) (Ret annz (Read annz "x")))))
      it "data X with Int ; X 1 <- X 1 ; return 1" $
        (parse' stmt "data Xxx with Int ; set Xxx 1 <- Xxx 1 ; return 1")
        `shouldBe` Right (Seq annz (Data annz (TypeD ["Xxx"] [] int,cz) False) (Seq annz (Set annz False (LCons ["Xxx"] (LNumber 1)) (Cons annz ["Xxx"] (Number annz 1))) (Ret annz (Number annz 1))))
      it "data X with Int ; x:Int ; X 1 <- X 2" $
        (parse' stmt "data Xxx with Int ; set Xxx 1 <- Xxx 2 ; return 2")
        `shouldBe` Right (Seq annz (Data annz (TypeD ["Xxx"] [] int,cz) False) (Seq annz (Set annz False (LCons ["Xxx"] (LNumber 1)) (Cons annz ["Xxx"] (Number annz 2))) (Ret annz (Number annz 2))))

      it "Aa <- Aa.Bb" $
        (parse' stmt $
          unlines [
            "data Aa with Int",
            "data Aa.Bb",
            "var b : Aa.Bb <- Aa.Bb 1",
            "var a : Aa <- b",
            "var v : Int",
            "set (Aa v) <- b",
            "return v"
          ])
        `shouldBe` Right (Seq annz (Data annz (TypeD ["Aa"] [] int,cz) False) (Seq annz (Data annz (TypeD ["Aa","Bb"] [] Type0,cz) False) (Seq annz (Seq annz (Seq annz (Var annz "b" (TypeD ["Aa","Bb"] [] Type0,cz)) (Nop annz)) (Set annz False (LVar "b") (Cons annz ["Aa","Bb"] (Number annz 1)))) (Seq annz (Seq annz (Seq annz (Var annz "a" (TypeD ["Aa"] [] Type0,cz)) (Nop annz)) (Set annz False (LVar "a") (Read annz "b"))) (Seq annz (Seq annz (Seq annz (Var annz "v" (int,cz)) (Nop annz)) (Nop annz)) (Seq annz (Set annz False (LCons ["Aa"] (LVar "v")) (Read annz "b")) (Ret annz (Read annz "v"))))))))

  describe "data - constraint:" $ do

      it "Unit a/IEq" $
        parse' stmt "data Unit for a with a where a is IEq"
        `shouldBe` Right (Data annz (TypeD ["Unit"] [TypeV "a"] (TypeV "a"),M.fromList [("a",S.fromList ["IEq"])]) False)
      it "Pair (a,a)" $
        parse' stmt "data Pair for a with (a,a)"
        `shouldBe` Right (Data annz (TypeD ["Pair"] [TypeV "a"] (TypeN [TypeV "a",TypeV "a"]),M.fromList []) False)
      it "Pair (a,a)/IEq" $
        parse' stmt "data Pair for a with (a,a) where (a is IEq)"
        `shouldBe` Right (Data annz (TypeD ["Pair"] [TypeV "a"] (TypeN [TypeV "a",TypeV "a"]),M.fromList [("a",S.fromList ["IEq"])]) False)
      it "Pair (a,b)" $
        parse' stmt "data Pair for (a,b) with (a,b)"
        `shouldBe` Right (Data annz (TypeD ["Pair"] [TypeV "a",TypeV "b"] (TypeN [TypeV "a",TypeV "b"]),M.fromList []) False)
      it "Pair (a,b)/IEq" $
        parse' stmt "data Pair for (a,b) with (a,b) where (a is IEq, b is IEq)"
        `shouldBe` Right (Data annz (TypeD ["Pair"] [TypeV "a",TypeV "b"] (TypeN [TypeV "a",TypeV "b"]),M.fromList [("a",S.fromList ["IEq"]),("b",S.fromList ["IEq"])]) False)

      it "Pair (a,a) ; p1:Pair(Int,Int)" $
        parse' stmt "data Pair for a with (a,a) ; var p1 : Pair of Int"
        `shouldBe` Right (Seq annz (Data annz (TypeD ["Pair"] [TypeV "a"] (TypeN [TypeV "a",TypeV "a"]),M.fromList []) False) (Seq annz (Seq annz (Var annz "p1" (TypeD ["Pair"] [int] Type0,M.fromList [])) (Nop annz)) (Nop annz)))

      it "Either" $
        parse' stmt "data Either for (a,b) ; data Either.Left  with a ; data Either.Right with b"
        `shouldBe` Right (Seq annz (Data annz (TypeD ["Either"] [TypeV "a",TypeV "b"] Type0,M.fromList []) False) (Seq annz (Data annz (TypeD ["Either","Left"] [] (TypeV "a"),M.fromList []) False) (Data annz (TypeD ["Either","Right"] [] (TypeV "b"),M.fromList []) False)))
-}
