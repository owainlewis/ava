module Language.Ava.Internal.Stack where
  ( spec, main )
where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Stack operations" $ do
    it "should work" $ do
      1 `shouldBe` 1
