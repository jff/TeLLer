module Arbitrary where
import Test.QuickCheck
import Syntax

 
instance Arbitrary Term where
        arbitrary
          = do x <- choose (0 :: Int, 12)
               case x of
                   0 -> do x1 <- arbitrary
                           return (Atom x1)
                   1 -> do x1 <- arbitrary
                           return (Not x1)
                   2 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return ((:*:) x1 x2)
                   3 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return ((:$:) x1 x2)
                   4 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return ((:-@:) x1 x2)
                   5 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return ((:&:) x1 x2)
                   6 -> do x1 <- arbitrary
                           x2 <- arbitrary
                           return ((:+:) x1 x2)
                   7 -> do x1 <- arbitrary
                           return (OfCourse x1)
                   8 -> do x1 <- arbitrary
                           return (WhyNot x1)
                   9 -> return Top
                   10 -> return Bottom
                   11 -> return One
                   12 -> return Zero
                   _ -> error "FATAL ERROR: Arbitrary instance, logic bug"
