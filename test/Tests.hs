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
    patternSpec


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
    describe "Geometry.move" $ do
        it "does nothing with no velocity" $ do
            move 1 noVelocity (position 1 1) `shouldBe` position 1 1
            move 1 noVelocity (position 0 0) `shouldBe` position 0 0
        it "does nothing with no time" $ do
            move 0 (velocity 10 10) (position 1 1) `shouldBe` position 1 1
            move 0 (velocity 10 10) (position 0 0) `shouldBe` position 0 0
    describe "Geometry.collides" $ do
        it "returns true for self collision" $
            collides 0 (position 0 0, look) (position 0 0, look) `shouldBe` True
        it "handles basic collisions" $ do
            collides 0 (position 0 20, look) (position 0 0, look) `shouldBe` True
            collides 0 (position 0 21, look) (position 0 0, look) `shouldBe` False
  where
    look = Look 10 SquareShape Blue
    cl10 = clamp 10 10


patternSpec =
    describe "Patterns.scaleTime" $ do
        it "does nothing for empty Paths" $ do
            scaleTime 0 mempty `shouldBe` mempty
            scaleTime 1 mempty `shouldBe` mempty
        it "does nothing with no time" $
            let p = divide 4 (position 1 1)
            in scaleTime 0 p `shouldBe` p
        it "is reversible" $
            let p = divide 1 (position 1 1)
            in (scaleTime (-1) . scaleTime 1) p `shouldBe` p