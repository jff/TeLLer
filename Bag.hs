module Bag where

import Test
import Test.QuickCheck (forAll)

import Data.Map (Map)
import qualified Data.Map as Map

type Bag a = Map a Int

subbag :: Ord a => Bag a -> Bag a -> Bool
subbag x y 
    | Map.null y = True
    | otherwise  = (not . Map.null) (Map.differenceWith f x y)
    where f x y = let z = x - y in
                    if z >= 0
                    then Just z
                    else Nothing

prop_subbag_reflexive = forAll bags $ \x -> subbag x x
