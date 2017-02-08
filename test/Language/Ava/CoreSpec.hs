module Language.Ava.CoreSpec
    ( spec
    , main
    ) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "This module" $ do
    it "should work" $ do
      1 `shouldBe` 1
