module Main where

import Bag
import Syntax
import Parser
import Printer
import Logic

main = getLine >>= run' term presentTerm >> main

presentTerm t = putStrLn (showTerms (map simplify t)) >> print t
