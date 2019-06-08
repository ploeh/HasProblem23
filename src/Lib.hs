module Lib where

import Data.List
import Data.Maybe
import System.Random

rndGenSelect' :: RandomGen g => g -> String -> Int -> String
rndGenSelect' _ _ count = replicate count 's'

rndGenSelect :: (RandomGen g, Integral i) => g -> [a] -> i -> [a]
rndGenSelect rnd xs count =
  let indices = genericTake count $ randomRs (0, length xs - 1) rnd
      indexedValues = zip [0..] xs
  in mapMaybe (`lookup` indexedValues) indices

rndSelect :: Integral i => [a] -> i -> IO [a]
rndSelect xs count = do
  rnd <- newStdGen
  return $ rndGenSelect rnd xs count
