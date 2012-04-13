module Test where

import Data.Map (Map)
import qualified Data.Map as Map

import Test.QuickCheck

bags :: Gen (Map Int Int)
bags = do
  let positivePairs
        :: (Arbitrary a1, Arbitrary b1, Ord b1, Num b1)
        => Gen (a1, Positive b1)
      positivePairs = arbitrary
  list <- listOf positivePairs
  let listNum = fmap (\(x, Positive y) -> (x, y)) list
  return (Map.fromList listNum)
