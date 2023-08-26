module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "test something" $ do
        it "should fucking work, or else..." $ do
            3 `shouldBe` 3
