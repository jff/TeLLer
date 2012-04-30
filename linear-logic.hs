module Main where

import Bag
import Syntax
import Parser
import Printer
import RewriteRules

main = getLine >>= run' term presentTerm >> main

presentTerm t = putStrLn (showTerms (map simplify t)) >> print t
