module Main where

import Bag
import Syntax
import Parser
import Printer

main = getLine >>= run' term (\t -> putStrLn (showTerms t) >> print t) >> main
