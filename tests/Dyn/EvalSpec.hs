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
          Dcl (az, PWrite az "b", Just tz, Just $ Where (az, EVar az "a",[])),
          Dcl (az, PWrite az "a", Just tz, Just $ Where (az, EUnit az,[]))
        ]))
        `shouldBe` (EUnit az)
    it "b where b::() b=()" $
      evalWhere []
        (Where (az, EVar az "b", [
          Dcl (az, PWrite az "b", Just tz, Nothing),
          Dcl (az, PWrite az "b", Nothing, Just $ Where (az, EUnit az,[]))
        ]))
        `shouldBe` (EUnit az)

  describe "evalDcl:" $ do
    it "a=()" $
      evalDcl [] (Dcl (az, PWrite az "a", Just tz, Just $ Where (az, EUnit az,[])))
        `shouldBe` ([("a", EUnit az)], Right True)

  describe "parser:" $ do
    it "error" $
      (evalProg $ fromRight $ parse "main = error")
        `shouldBe` (EError az{pos=(1,8)} "<user>")
    it "match-true" $
      (evalProg $ fromRight $ parse "main = if () ~ () then () else error;")
        `shouldBe` (EUnit az{pos=(1,24)})
    it "match-false" $
      (evalProg $ fromRight $ parse "main = if () ~ A then error else ();")
        `shouldBe` (EUnit az{pos=(1,34)})
    it "match-error" $
      (evalProg $ fromRight $ parse "main = if error ~ () then () else error;")
        `shouldBe` (EError az{pos=(1,11)} "<user>")
    it "assign-var" $
      (evalProg $ fromRight $ parse "main = a where a = A;")
        `shouldBe` EData az{pos=(1,20)} ["A"] (EUnit az{pos=(1,20)})
    it "assign-tuple" $
      (evalProg $ fromRight $ parse "main = (a,b) where (a,b) = (A,B);")
        `shouldBe` ETuple az{pos=(1,8)} [EData az{pos=(1,29)} ["A"] (EUnit az{pos=(1,29)}),EData az{pos=(1,31)} ["B"] (EUnit az{pos=(1,31)})]

  describe "run:"$ do
    it "(" $
      run "main = (" `shouldBe` "(line 1, column 9):\nunexpected end of input\nexpecting expression"
    it "()" $
      run "main = ()" `shouldBe` "()"
    it "f ()" $
      run "main = f () where f = func () ...;;" `shouldBe` "()"
    it "Xx a = ()" $
      run "main = a where Xx a = Xx ();" `shouldBe` "()"
