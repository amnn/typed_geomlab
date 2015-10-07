module TokenSpec where

import SpecHelper

import Token

spec :: Spec
spec = do
  describe "assoc" $ do
    it "assigns higher priority to * over +" $ do
      let plusP = pri (assoc "+")
      let multP = pri (assoc "*")
      (plusP, multP) `shouldSatisfy` uncurry (<)
  describe "fixPrec" $ do
    it "binds * tighter than +" $ do
       let t1 = Op "*" (Op "+" (Leaf 1) (Leaf 2)) (Leaf 3) :: OpTree Int
       let t2 = Op "+" (Leaf 1) (Op "*" (Leaf 2) (Leaf 3)) :: OpTree Int
       fixPrec t1 `shouldBe` t2
       fixPrec t2 `shouldBe` t2
