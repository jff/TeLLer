{-# LANGUAGE ExistentialQuantification #-}
module TestRunner where

import Test
import Test.QuickCheck

import Bag

data Test = forall a. (Testable a) => Test a

tests :: [Test]
tests = [
    Test prop_printParse,
    Test prop_subbag_reflexive,
    Test prop_subbag_less,
    Test prop_subbag_more
  ]

main = sequence_ (map runCheck tests)
     where runCheck (Test t) = quickCheck t
