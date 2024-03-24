module Main where

import Data.List (genericLength)
import System.Random
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit
import Test.HUnit.Base
import Test.QuickCheck
import Lib

lenProp :: (Integral i, Show i) => Int -> NonEmptyList a -> NonNegative i -> Property
lenProp seed (NonEmpty xs) (NonNegative i) =
  i === genericLength (rndGenSelect (mkStdGen seed) xs i)

negLenProp :: Integral i => Int -> [a] -> Positive i -> Property
negLenProp seed xs (Positive i) =
  0 === genericLength (rndGenSelect (mkStdGen seed) xs (-i))

emptyListProp :: Integral i => Int -> NonNegative i -> Property
emptyListProp seed (NonNegative i) =
  0 === genericLength (rndGenSelect (mkStdGen seed) [] i)

main :: IO ()
main = defaultMain tests

tests = [
    testGroup "Properties" [
      testProperty "rndGenSelect returns result of correct length" (lenProp :: Int -> NonEmptyList Int -> NonNegative Int -> Property),
      testProperty "rndGenSelect returns result of correct length" (lenProp :: Int -> NonEmptyList Char -> NonNegative Integer -> Property),
      testProperty "rndGenSelect returns empty result when count is negative" (negLenProp :: Int -> [Int] -> Positive Int -> Property),
      testProperty "rndGenSelect returns empty result when input is empty" (emptyListProp :: Int -> NonNegative Int -> Property)
    ],
    testGroup "Regression tests" $ hUnitTestToTests $ TestList [
      "rndGenSelect of chars returns correct result" ~: do
        (seed, xs, count, expected) <-
          [
            (     42,      "foo",  3,        "ffo"),
            (   1337,      "bar", 10, "rabraraabb"),
            (-197221, ['a'..'z'],  5,      "oggab")
          ]
        let rnd = mkStdGen seed

        let actual = rndGenSelect rnd xs count

        return $ expected ~=? actual

      ,
      "rndGenSelect of integers returns correct result" ~: do
        (seed, xs, count, expected) <-
          [
            (  19,      [1..3],  3,               [3,1,2]),
            (1770, [0,1,1,2,7], 10, [7,7,1,0,2,1,1,7,1,0]),
            ( -19,     [0..99],  5,       [79,55,31,38,94])
          ]
        let rnd = mkStdGen seed

        let actual = rndGenSelect rnd xs count

        return $ expected ~=? actual

      ,
      "rndSelect of empty string returns zero elements" ~: do
        actual <- rndSelect "" 1
        return $ "" @=? actual
    ]
  ]
