module DynArraySpec where

import SpecHelper
import Control.Monad
import Control.Monad.ST
import DynArray

spec :: Spec
spec = do
  describe "push" $ do
    it "adds elements" $ do
      let res = runST $ do { d <- newArray_ 10
                           ; push d 1
                           ; peek 1 d
                           }
      (res :: Int) `shouldBe` 1

  describe "pop" $ do
    it "removes elements" $ do
      let res = runST $ do { d <- newArray_ 4
                           ; forM_ [1..3] (push d)
                           ; pop d
                           ; peek 1 d
                           }
      (res :: Int) `shouldBe` 2

  describe "fetch" $ do
   it "accesses random elements" $ do
     let res = runST $ do { d <- newArray_ 10
                          ; forM_ [1..10] (push d)
                          ; forM  [0..9]  (fetch d)
                          }
     (res :: [Int]) `shouldBe` [1..10]

  describe "expansion" $ do
    it "preserves contents" $ do
      let res = runST $ do { d <- newArray_ 1
                           ; forM_ ([1, 2]) (push d)
                           ; replicateM 2 (pop' d)
                           }
      (res :: [Int]) `shouldBe` [2, 1]

  describe "compaction" $ do
    it "preserves contents" $ do
      let res = runST $ do { d <- newArray_ 8
                           ; forM_ [1..8] (push d)
                           ; replicateM_ 7 (pop d)
                           ; peek 1 d
                           }
      (res :: Int) `shouldBe` 1
