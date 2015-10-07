module LiteralSpec where

import SpecHelper

spec :: Spec
spec = do
  describe "Literal" $ do
    it "tests tests" $ do
      (1 :: Int) `shouldBe` (1 :: Int)
