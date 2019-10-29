module Dyn.EvalSpec (main,spec) where

import Test.Hspec

import Dyn.AST
import Dyn.Parser
import Dyn.Eval

fromRight (Right x) = x

main :: IO ()
main = hspec spec

spec = do

  describe "evalExpr:" $ do
    it "()" $
      evalExpr [] (EUnit az) `shouldBe` (EUnit az)
    it "a" $
      evalExpr [("a",EUnit az)] (EVar az "a") `shouldBe` (EUnit az)

  describe "evalWhere:" $ do
    it "()" $
      evalWhere []
        (Where (az, EUnit az, []))
        `shouldBe` (EUnit az)
    it "b where b=a, a=()" $
      evalWhere []
        (Where (az, EVar az "b", [
          DAtr az (PWrite az "b") (Where (az, EVar az "a",[])),
          DAtr az (PWrite az "a") (Where (az, EUnit az,[]))
        ]))
        `shouldBe` (EUnit az)
    it "b where b::() b=()" $
      evalWhere []
        (Where (az, EVar az "b", [
          DSig az "b" tz,
          DAtr az (PWrite az "b") (Where (az, EUnit az,[]))
        ]))
        `shouldBe` (EUnit az)

  describe "evalDecl:" $ do
    it "a=()" $
      evalDecl [] (DAtr az (PWrite az "a") (Where (az, EUnit az,[])))
        `shouldBe` ([("a", EUnit az)], Right True)

  describe "parser:" $ do
    it "error" $
      (evalProg False $ fromRight $ parse "main = error")
        `shouldBe` (EError az{pos=(1,8)} "<user>")
    it "match-true" $
      (evalProg True $ fromRight $ parse "main = case () of () -> ()\n_ -> error;")
        `shouldBe` (EUnit az{pos=(1,25)})
    it "match-false" $
      (evalProg False $ fromRight $ parse "main = case () of A -> error\n_ -> ();")
        `shouldBe` (EUnit az{pos=(2,6)})
    it "match-error" $
      (evalProg True $ fromRight $ parse "main = case error of ()->() \n _->error;")
        `shouldBe` (EError az{pos=(1,13)} "<user>")
    it "match-error" $
      (evalProg False $ fromRight $ parse "main = case () of A->();")
        `shouldBe` EError (Ann {pos = (1,8)}) "non-exhaustive patterns"
    it "assign-var" $
      (evalProg True $ fromRight $ parse "main = a where a = A;")
        `shouldBe` EData az{pos=(1,20)} ["A"] (EUnit az{pos=(1,20)})
    it "assign-tuple" $
      (evalProg False $ fromRight $ parse "main = (a,b) where (a,b) = (A,B);")
        `shouldBe` ETuple az{pos=(1,8)} [EData az{pos=(1,29)} ["A"] (EUnit az{pos=(1,29)}),EData az{pos=(1,31)} ["B"] (EUnit az{pos=(1,31)})]

  describe "evalString:" $ do
    it "(" $
      evalString False "main = (" `shouldBe` "(line 1, column 9):\nunexpected end of input\nexpecting expression"
    it "()" $
      evalString True "main = ()" `shouldBe` "()"
    it "f ()" $
      evalString False "main = f () where f = func :: () -> ...;;" `shouldBe` "()"
    it "Xx a = ()" $
      evalString True "main = a where Xx a = Xx ();" `shouldBe` "()"
    it "patt - (x)" $
      evalString False "main = x where (x) = ();" `shouldBe` "()"
    it "patt - read - fail" $
      evalString True "main = case () of ~func->(); -> ();" `shouldBe` "(line=1, col=19) ERROR : invalid pattern : \"func :: () ->\\n  ()\\n;\""
    it "patt - read - ok" $
      evalString False "main = case () of ~() -> ();" `shouldBe` "()"
    it "patt - fail" $
      evalString True "main = x where Xxx = Yyy;" `shouldBe` "(line=1, col=16) ERROR : invalid assignment"
    it "patt - fail" $
      evalString False "main = case Xxx of Yyy->Xxx;" `shouldBe` "(line=1, col=8) ERROR : non-exhaustive patterns"
    it "patt - read - ok" $
      evalString True "main = case ((),()) of ~((),()) -> ();" `shouldBe` "()"
    it "f ()" $
      evalString False "main = f () \n f = func :: ()-> ...;" `shouldBe` "()"
    it "Nat" $
      evalString True "main = Nat.Succ Nat.Zero"
        `shouldBe` "(Nat.Succ Nat.Zero)"
    it "..." $
      evalString False "main = ... where ... = Nat.Zero;"
        `shouldBe` "Nat.Zero"
