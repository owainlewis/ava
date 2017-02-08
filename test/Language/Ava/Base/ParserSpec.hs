{-# LANGUAGE OverloadedStrings #-}
module Language.Ava.Base.ParserSpec ( spec ) where

import qualified Language.Ava.Base.Parser as Parser

import Test.Hspec
import Data.Either(isRight)

main :: IO ()
main = hspec spec

shouldParse p i = isRight (Parser.readExpr p i) `shouldBe` True

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "should parse an integer" $ do
      shouldParse Parser.parseInteger "1"
      shouldParse Parser.parseInteger "123"
    it "should parse a float" $ do
      shouldParse Parser.parseFloat "1.0"
      shouldParse Parser.parseFloat "2.50"
    it "should parse a boolean" $ do
      shouldParse Parser.parseBoolean "true"
      shouldParse Parser.parseBoolean "false"
    it "should parse a string" $ do
      shouldParse Parser.parseString "\"Hello, World!\""
    it "should parse a list" $ do
      shouldParse Parser.parseList "[]"
      shouldParse Parser.parseList "[1,2,3]"
      shouldParse Parser.parseList "[1, 2, 3]"
    it "should parse a quotation" $ do
      shouldParse Parser.parseQuotation "{}"
      shouldParse Parser.parseQuotation "{ 1 2 3 }"
    it "should parse a function definition" $ do
      shouldParse Parser.parseProcedure $ do
        shouldParser Parser.parseProcedure "define foo { bar }"
        shouldParser Parser.parseProcedure "define bar { }"
