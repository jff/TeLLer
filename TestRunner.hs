{-# LANGUAGE ExistentialQuantification #-}
module TestRunner where

import Test
import Test.QuickCheck

import Logic
import Bag

data Test = forall a. (Testable a) => Test String a

tests :: [Test]
tests = [
    Test "Simplify -- Idempotent"
         prop_simplify_idempotent,
    Test "Simplify -- Only negative atoms"
         prop_simplify_onlyNegativeAtoms,
    Test "Simplify -- In Exponential Lattice"
         prop_simplify_inExponentialLattice,
    Test "Subbag -- Reflexive" prop_subbag_reflexive,
    Test "Subbag -- Less" prop_subbag_less,
    Test "Subbag -- More" prop_subbag_more,
    Test "Parser -- Print and Parse is Identity" prop_printParse_ident
  ]

main = sequence_ (map runCheck tests)
  where runCheck (Test s t) =
          putStrLn s >> quickCheckWith (stdArgs {maxSuccess = 10000}) t
