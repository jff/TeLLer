module Test where

import Data.Map (Map)
import qualified Data.Map as Map

import Test.QuickCheck

bags :: Gen (Map Int Int)
bags = do
  let positivePairs = arbitrary :: Gen (Int, Positive Int)
  list <- listOf positivePairs
  let listNum = map (\(x, Positive y) -> (x, y)) list
  return (Map.fromList listNum)
