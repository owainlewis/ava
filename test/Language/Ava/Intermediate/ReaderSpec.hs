module Language.Ava.Base.ReaderSpec
  ( spec, main )
where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Reading intermediate forms" $ do
    it "should work" $ do
      1 `shouldBe` 1
