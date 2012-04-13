module Test where

import Test.QuickCheck
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

prop_PrintParse :: [Term] -> Bool
prop_PrintParse t  =
  let str = showTerms t in
    case parse Parser.term "<quickcheck>" str of
      Left  a  -> False
      Right t' -> t == t'
