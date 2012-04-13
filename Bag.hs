module Bag where

import Test
import Test.QuickCheck (forAll, (==>))

import Data.Map (Map)
import qualified Data.Map as Map

type Bag a = Map a Int

difference :: Ord a => Bag a -> Bag a -> Bag a
difference x y = Map.differenceWith f x y
           where f x y = if x - y /= 0
                       then Just (x - y)
                       else Nothing

valid :: (Ord a) => Bag a -> Bool
valid x = Map.fold (\x a -> a && x >= 0) True x

subbag :: (Ord a) => Bag a -> Bag a -> Bool
subbag x y = valid (difference x y)

prop_subbag_reflexive = forAll bags $ \x -> subbag x x

prop_subbag_less = forAll bags $ \x ->
                     let y = Map.map (\n -> n - 1) x
                     in subbag x y

prop_subbag_more = forAll bags $ \x ->
                     let y = Map.map (+ 1) x
                     in not (Map.null y) ==> not (subbag x y)

