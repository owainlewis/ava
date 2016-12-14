module Language.Seven.ParserSpec ( spec ) where


import qualified Language.Seven.Parser as Parser

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parsing" $ do
    it "should parse numbers" $ do
      head [23 ..] `shouldBe` (23 :: Int)