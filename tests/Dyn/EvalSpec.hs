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
          Dcl (az, EVar az "b", Just tz, Just $ Where (az, EVar az "a",[])),
          Dcl (az, EVar az "a", Just tz, Just $ Where (az, EUnit az,[]))
        ]))
        `shouldBe` (EUnit az)
    it "b where b::() b=()" $
      evalWhere []
        (Where (az, EVar az "b", [
          Dcl (az, EVar az "b", Just tz, Nothing),
          Dcl (az, EVar az "b", Nothing, Just $ Where (az, EUnit az,[]))
        ]))
        `shouldBe` (EUnit az)

  describe "evalDcl:" $ do
    it "a=()" $
      evalDcl [] (Dcl (az, EVar az "a", Just tz, Just $ Where (az, EUnit az,[])))
        `shouldBe` ([("a", EUnit az)], Right True)

  describe "parser:" $ do
    it "error" $
      (evalProg $ fromRight $ parse "error")
        `shouldBe` (EError az{pos=(1,1)})
    it "match-true" $
      (evalProg $ fromRight $ parse "if () ~ () then () else error")
        `shouldBe` (EUnit az{pos=(1,17)})
    it "match-false" $
      (evalProg $ fromRight $ parse "if () ~ A then error else ()")
        `shouldBe` (EUnit az{pos=(1,27)})
    it "match-error" $
      (evalProg $ fromRight $ parse "if error ~ () then () else error")
        `shouldBe` (EError az{pos=(1,4)})
    it "assign-var" $
      (evalProg $ fromRight $ parse "a where (a = A)")
        `shouldBe` EData az{pos=(1,14)} ["A"] (EUnit az{pos=(1,14)})
    it "assign-tuple" $
      (evalProg $ fromRight $ parse "(a,b) where (a,b) = (A,B);")
        `shouldBe` ETuple az{pos=(1,1)} [EData az{pos=(1,22)} ["A"] (EUnit az{pos=(1,22)}),EData az{pos=(1,24)} ["B"] (EUnit az{pos=(1,24)})]

  describe "run:"$ do
    it "(" $
      run "(" `shouldBe` "(line 1, column 2):\nunexpected end of input\nexpecting expression"
    it "()" $
      run "()" `shouldBe` "()"
    it "f ()" $
      run "f () where (f = func () ...)" `shouldBe` "()"
    it "Xx a = ()" $
      run "a where (Xx a = Xx ())" `shouldBe` "()"
