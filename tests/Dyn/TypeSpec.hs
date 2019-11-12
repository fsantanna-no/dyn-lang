{-# LANGUAGE QuasiQuotes #-}

module Dyn.TypeSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import qualified Dyn.Eval as E
import qualified Dyn.Analyse as A

evalString = E.evalStringF A.apply

main :: IO ()
main = hspec spec

spec = do

  describe "Type" $ do

    it "main=Type Bool.True" $
      evalString ("main=Type Bool.True")
        `shouldBe` "(Type Bool.True)"

  describe "Infer" $ do

    it "main=Type x=Bool.True" $
      evalString ("main=Type x where x=Bool.True ;")
        `shouldBe` "(Type Bool.True)"

    it "main=Type (x,y)=(Bool.True,())" $
      evalString ("main=Type (x,y) where (x,y)=(Bool.True,()) ;")
        `shouldBe` "(Type (Bool.True,()))"
