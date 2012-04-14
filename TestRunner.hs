{-# LANGUAGE ExistentialQuantification #-}
module TestRunner where

import Test
import Test.QuickCheck

import Logic
import Bag

data Test = forall a. (Testable a) => Test a

tests :: [Test]
tests = [
    Test prop_simplify_idempotent,
    Test prop_simplify_onlyNegativeAtoms,
    Test prop_simplify_inExponentialLattice,
    Test prop_subbag_reflexive,
    Test prop_subbag_less,
    Test prop_subbag_more,
    Test prop_printParse_ident
  ]

main = sequence_ (map runCheck tests)
     where runCheck (Test t) = quickCheck t
