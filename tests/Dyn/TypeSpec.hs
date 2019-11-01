{-# LANGUAGE QuasiQuotes #-}

module Dyn.TypeSpec (main,spec) where

import Test.Hspec
import Text.RawString.QQ

import Dyn.AST
import Dyn.Eval
import Dyn.Type

main :: IO ()
main = hspec spec

spec = do

  describe "Type" $ do

    it "main=Type x x=Bool.True" $
      evalString True ("main = x where x=Bool.true")
        `shouldBe` "Bool.True"
