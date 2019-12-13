module Dyn.CCSpec (main,spec) where

import Test.Hspec

import Dyn.AST
import Dyn.Parser
import Dyn.CC
import Dyn.Prelude
import qualified Dyn.Order as Order (apply)

fromRight (Right x) = x

main :: IO ()
main = hspec spec

spec = do

  describe "ccExpr:" $ do
    it "error" $
      ccExpr (EError pz "xxx") `shouldBe` "error_at_line(0,0,NULL,0,\"%s\",\"xxx\")"
    it "a" $
      ccExpr (EVar pz "a") `shouldBe` "a"

  describe "ccExpWhere:" $ do
    it "a" $
      ccExpWhere
        (ExpWhere (pz, [], (EVar pz "a")))
        `shouldBe` ([],"a")
    it "b where b=a, a=()" $
      ccExpWhere
        (ExpWhere (pz, [
          DAtr pz (PWrite pz "b") (ExpWhere (pz, [], EVar pz "a")),
          DAtr pz (PWrite pz "a") (ExpWhere (pz, [], EUnit pz))
        ], EVar pz "b"))
        `shouldBe` ([DAtr pz (PWrite pz "b") (ExpWhere (pz,[],EVar pz "a")),DAtr pz (PWrite pz "a") (ExpWhere (pz,[],EUnit pz))], "b")
    it "b where b::() b=()" $
      ccExpWhere
        (ExpWhere (pz, [
          DSig pz "b" cz TAny,
          DAtr pz (PWrite pz "b") (ExpWhere (pz,[], EUnit pz))
        ], EVar pz "b"))
        `shouldBe` ([DSig pz "b" cz TAny,DAtr pz (PWrite pz "b") (ExpWhere (pz,[],EUnit pz))],"b")

  describe "match:" $ do
    it "_=()" $
      match (PAny pz) "()"
        `shouldBe` "true"
    it "...=a" $
      match (PArg pz) "a"
        `shouldBe` "(_ceu_arg = a , true)"

  describe "ccDecl:" $ do
    it "a=b" $
      ccDecl (DAtr pz (PWrite pz "a") (ExpWhere (pz,[], EVar pz "b")))
        `shouldBe` "(a = b , true)"

{-
  describe "parser:" $ do
    it "error" $
      (evalProg $ fromRight $ parse "main = error;")
        `shouldBe` (EError (1,8) "<user>")
    it "match-true" $
      (evalProg $ fromRight $ parse "main = case () { () -> ();\n_ -> error;};")
        `shouldBe` (EUnit (1,24))
    it "match-false" $
      (evalProg $ fromRight $ parse "main = case () { A -> error;\n_ -> ();};")
        `shouldBe` (EUnit (2,6))
    it "match-error" $
      (evalProg $ fromRight $ parse "main = case error { ()->(); \n _->error;};")
        `shouldBe` (EError (1,13) "<user>")
    it "match-error" $
      (evalProg $ fromRight $ parse "main = case () { A->();};")
        `shouldBe` EError (1,8) "non-exhaustive patterns"
    it "assign-var" $
      (evalProg $ fromRight $ parse "main = a where{a = A;};")
        `shouldBe` EData (1,20) ["A"] (EUnit (1,20))
    it "assign-var" $
      (evalProg $ fromRight $ parse "main = let a=A; {  a};")
        `shouldBe` EData (1,14) ["A"] (EUnit (1,14))
    it "assign-tuple" $
      (evalProg $ fromRight $ parse "main = (a,b) where{(a,b) = (A,B);};")
        `shouldBe` ETuple (1,8) [EData (1,29) ["A"] (EUnit (1,29)),EData (1,31) ["B"] (EUnit (1,31))]

  describe "evalString:" $ do
    it "(" $
      evalString "main = (" `shouldBe` "(line 1, column 9):\nunexpected end of input\nexpecting expression"
    it "()" $
      evalString "main = ();" `shouldBe` "()"
    it "f ()" $
      evalString "main = f () where { f = func :: () { ... };};" `shouldBe` "()"
    it "Xx a = ()" $
      evalString "main = a where { Xx a = Xx ();};" `shouldBe` "()"
    it "sub" $
      evalString "main = a where { Xx a = Xx.Yy ((),());};" `shouldBe` "()"
    it "patt - (x)" $
      evalString "main = x where { (x) = ();};" `shouldBe` "()"
    it "patt - read - fail" $
      evalString "main = case () { ~func{()} -> ();};" `shouldBe` "(line=1, col=18) ERROR : invalid pattern : \"(func :: ? {\\n  ()\\n})\""
    it "patt - read - ok" $
      evalString "main = case () { ~() -> ();};" `shouldBe` "()"
    it "patt - fail" $
      evalString "main = x where { Xxx = Yyy;};" `shouldBe` "(line=1, col=18) ERROR : invalid assignment"
    it "patt - fail" $
      evalString "main = case Xxx { Yyy->Xxx;};" `shouldBe` "(line=1, col=8) ERROR : non-exhaustive patterns"
    it "patt - read - ok" $
      evalString "main = case ((),()) { ~((),()) -> ();};" `shouldBe` "()"
    it "f ()" $
      evalString "main = f (); \n f = func :: (){ ... };" `shouldBe` "()"
    it "Nat" $
      evalString "main = Nat.Succ Nat.Zero;"
        `shouldBe` "(Nat.Succ Nat.Zero)"
    it "..." $
      evalString "main = ... where { ... = Nat.Zero;};"
        `shouldBe` "Nat.Zero"

  describe "misc:" $ do
    describe "char:" $ do
      it "ord" $
        evalString ("main = ord Char.AA;"++char++nat) `shouldBe` "(Nat.Succ Nat.Zero)"
      it "chr" $
        evalString ("main = chr one;"    ++char++nat) `shouldBe` "Char.AA"
    describe "list:" $ do
      it "[(),1,True]" $
        evalString ("main = List.Cons ((), List.Cons (Nat.Succ Nat.Zero, List.Cons (Bool.True, List.Nil)));")
          `shouldBe` "(List.Cons ((),(List.Cons ((Nat.Succ Nat.Zero),(List.Cons (Bool.True,List.Nil))))))"

  describe "order:" $ do
    it "a second" $
      evalStringF Order.apply ("main = a;\na = X;")
        `shouldBe` "X"
    it "a first" $
      evalStringF Order.apply ("a = X;\nmain = a;")
        `shouldBe` "X"
-}
