{-# LANGUAGE QuasiQuotes #-}

module ParserSpec where

import Test.Hspec

import qualified Text.Parsec as P (eof, parse)
import Text.Parsec.String (Parser)

import qualified Dyn.Parser  as D

parse :: Parser a -> String -> Either String a
parse rule input =
  case P.parse (rule <* P.eof) "" input of
    (Right v) -> Right v
    (Left  v) -> Left (show v)

main :: IO ()
main = hspec $ do
  describe "tokens:" $ do
    describe "comm:" $ do
      it "-- xxx " $
        parse D.tk_comm "-- xxx "
          `shouldBe` Right ()
    describe "var:" $ do
      it "xxx " $
        parse D.tk_var "xxx "
          `shouldBe` Right "xxx"

