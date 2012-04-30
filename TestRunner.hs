{-# LANGUAGE ExistentialQuantification #-}
module TestRunner(main) where

import System (getArgs)
import Control.Arrow ((>>>))

import Test
import Test.QuickCheck

import RewriteRules
import Bag

data Test = forall a. (Testable a) => Test String String a

tests :: [Test]
tests = [
    Test "Simplify" "Idempotent"
         prop_simplify_idempotent,
    Test "Dedual" "Only negative atoms"
         prop_dedual_onlyNegativeAtoms,
    Test "Unexpt" "In Exponential Lattice"
         prop_unexpt_inExponentialLattice1,
    Test "Unexpt" "In Exponential Lattice"
         prop_unexpt_inExponentialLattice2,
    Test "Subbag" "Reflexive" prop_subbag_reflexive,
    Test "Subbag" "Less" prop_subbag_less,
    Test "Subbag" "More" prop_subbag_more,
    Test "Parser" "Print and Parse is Identity" prop_printParse_ident
  ]

runTest :: Test -> IO ()
runTest (Test mod name prop) = do
  let title = concat [mod, " -- ", name]
  putStrLn title
  quickCheckWith (stdArgs {maxSuccess = 10000}) prop

runTests :: [Test] -> IO ()
runTests tests = mapM_ runTest tests

modname :: Test -> String
modname (Test mod _ _) = mod

main = do
     args <- getArgs
     case args of
          [m] -> runTests $ filter (modname >>> (== m)) tests
          _   -> runTests tests
