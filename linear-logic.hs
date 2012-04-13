module Main where

import Bag
import Syntax
import Parser

main = getLine >>= run term >> main
