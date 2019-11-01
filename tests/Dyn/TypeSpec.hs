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

    it "main=Type Bool.True" $
      evalString True ("main=TType Bool.True")
        `shouldBe` "(TType Bool.True)"

    it "main=Type x x=Bool.True" $
      evalString True ("main=TType x where x=Bool.True ;")
        `shouldBe` "(TType Bool.True)"
