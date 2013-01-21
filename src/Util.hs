module Util where

import Data.Traversable
import Control.Applicative
import System.IO (hFlush, stdout)

import Syntax

termMap :: (Term -> Term) -> Term -> Term
termMap f t = go t
  where go a@(Atom _)    = f a
        go (Not t)       = Not (go t)
        go (OfCourse t)  = OfCourse (go t)
        go (WhyNot t)    = WhyNot (go t)
        go (l :*: r)     = go l :*: go r
        go (l :$: r)     = go l :$: go r
        go (l :-@: r)    = go l :-@: go r
        go (l :&: r)     = go l :&: go r
        go (l :+: r)     = go l :+: go r
        go t | unitary t = t


------------------
-- IO utilities --
------------------
flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

flushStrLn :: String -> IO ()
flushStrLn s = putStrLn s >> hFlush stdout


