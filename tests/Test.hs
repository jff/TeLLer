module Test where

import Test.QuickCheck
import Debug.Trace
import Arbitrary

import Data.Map (Map)
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec

import Syntax
import Parser
import Printer

bags :: Gen (Map Int Int)
bags = do
  let positivePairs = arbitrary :: Gen (Int, Positive Int)
  list <- listOf positivePairs
  let listNum = map (\(x, Positive y) -> (x, y)) list
  return (Map.fromList listNum)

prop_printParse_ident :: [Term] -> Bool
prop_printParse_ident t  =
  let str = showTerms t in
    case parse Parser.term "<quickcheck>" str of
      Left err -> False
      Right t' -> t == t'
