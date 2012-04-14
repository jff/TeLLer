module Rewrite(rewrite, match) where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad (liftM2, mplus, msum)
import Data.Maybe (isJust)

import Arbitrary

import Parser
import Syntax
import Util

type Rule = (Term, Term)
type Binding = Map String Term

rewrite :: [Rule] -> Term -> Term
rewrite rules t = case msum matches of
                    Just t' -> rewrite rules t'
                    Nothing -> t
  where matches = map (\rule -> matchbind rule t) rules

rewrite' :: [Rule] -> Term -> Term
rewrite' rules t = recur (rewrite rules t)
  where walk t = rewrite' rules t
        recur a@(Atom _)   = a
        recur (Not t)      = Not $ walk t
        recur (OfCourse t) = OfCourse $ walk t
        recur (WhyNot t)   = WhyNot   $ walk t
        recur (a :*: b)    = walk a :*:  walk b
        recur (a :$: b)    = walk a :$:  walk b
        recur (a :-@: b)   = walk a :-@: walk b
        recur (a :&: b)    = walk a :&:  walk b
        recur (a :+: b)    = walk a :+:  walk b

matchbind :: (Term, Term) -> Term -> Maybe Term
matchbind (pattern, replace) t = 
  do binding <- match pattern t
     return (bind binding replace)

union :: Ord a => Maybe (Map a b) -> Maybe (Map a b) -> Maybe (Map a b)
union = Control.Monad.liftM2 Map.union

crossMatch (a, b) (c, d) = x `mplus` y
        where
           x = match a c `union` match b d
           y = match a d `union` match b c

match :: Term -> Term -> Maybe Binding

match (Atom s) t = Just (Map.singleton s t)
match (Not a)      (Not b)      = match a b
match (OfCourse a) (OfCourse b) = match a b
match (WhyNot a)   (WhyNot b)   = match a b

match (a :*: b) (c :*: d) = crossMatch (a, b) (c, d)
match (a :$: b) (c :$: d) = crossMatch (a, b) (c, d)
match (a :&: b) (c :&: d) = crossMatch (a, b) (c, d)
match (a :+: b) (c :+: d) = crossMatch (a, b) (c, d)

match (a :-@: b) (c :-@: d) = match a c `union` match b d

match Top Top       = Just Map.empty
match Bottom Bottom = Just Map.empty
match One One       = Just Map.empty
match Zero Zero     = Just Map.empty

match _ _ = Nothing

prop_match_reflixish t = isJust (match t t)

bind :: Binding -> Term -> Term
bind b t = termMap binding t
  where binding a@(Atom s) =
          case Map.lookup s b of
            Just t  -> t
            Nothing -> a
