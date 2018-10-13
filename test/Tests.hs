module Main
    ( main
    )
where

import Test.Hspec
import Test.DocTest (doctest)

import Game.Logic


main :: IO ()
main = do
    doctest ["src"]
    hspec spec

spec = do
    geometrySpec


geometrySpec = do
    describe "Geometry.clamp" $ do
        it "does nothing for values in range" $ do
            cl10 (position 1 1) `shouldBe` position 1 1
            cl10 (position 2 2) `shouldBe` position 2 2
        it "does nothing for values on the edge" $ do
            cl10 (position 0 0)   `shouldBe` position 0 0
            cl10 (position 10 10) `shouldBe` position 10 10
        it "sets negative values to zero" $ do
            cl10 (position (-1) (-1)) `shouldBe` position 0 0
            cl10 (position (-1) 1)    `shouldBe` position 0 1
            cl10 (position 1 (-1))    `shouldBe` position 1 0
        it "sets excessive values to the maximum" $ do
            cl10 (position 100 100) `shouldBe` position 10 10
            cl10 (position 100 0)   `shouldBe` position 10 0
            cl10 (position 0 100)   `shouldBe` position 0 10
  where
    cl10 = clamp 10 10