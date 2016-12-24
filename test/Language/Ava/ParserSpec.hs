{-# LANGUAGE OverloadedStrings #-}
module Language.Ava.ParserSpec ( spec ) where

import qualified Language.Ava.Parser as Parser

import Test.Hspec

main :: IO ()
main = hspec spec

--isSuccess :: Either a b
isSuccess (Left _) = False
isSuccess (Right _) = True

shouldParse p i = isSuccess (Parser.readExpr p i) `shouldBe` True

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "should parse an integer" $ do
      shouldParse Parser.parseInteger "1"
    it "should parse a float" $ do
      shouldParse Parser.parseFloat "1.0"
    it "should parse a boolean" $ do
      shouldParse Parser.parseBoolean "true"
      shouldParse Parser.parseBoolean "false"
    it "should parse a string" $ do
      shouldParse Parser.parseString "\"Hello, World!\""
    it "should parse a vector" $ do
      shouldParse Parser.parseVector "[1,2,3]"
