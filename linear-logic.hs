import Data.Map (Map)
import qualified Data.Map as Map

import Test.QuickCheck

type Bag a = Map a Int

main = undefined

subbag :: Ord a => Bag a -> Bag a -> Bool
subbag x y 
    | Map.null y = True
    | otherwise  = (not . Map.null) (Map.differenceWith f x y)
    where f x y = let z = x - y in
                    if z >= 0
                    then Just z
                    else Nothing

prop_subbag_reflexive x  =  subbag x x == True

instance (Ord a, Ord b, Num b, Arbitrary a, Arbitrary b) =>
         Arbitrary (Map a b) where
    arbitrary = Test.QuickCheck.listOf 
                (arbitrary :: (Arbitrary a1, Arbitrary b1, Ord b1, Num b1) 
                           => Gen (a1, Positive b1))
              >>= (return . map (\(x, Positive y) -> (x, y)))
              >>= (return . Map.fromList)
