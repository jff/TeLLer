module Main where

import Bag
import Syntax
import Parser
import Printer

main = getLine >>= run term >> main
