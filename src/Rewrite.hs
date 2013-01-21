module Rewrite (rewrite, rewrite', match, Rule) where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad (liftM2, mplus, msum, guard)
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
rewrite' rules t = rewrite rules (recur (rewrite rules t))
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
        recur t            = t

matchbind :: (Term, Term) -> Term -> Maybe Term
matchbind (pattern, replace) t =
  do binding <- match pattern t
     return (bind binding replace)

merge :: (Ord a, Eq b) => Maybe (Map a b) -> Maybe (Map a b) -> Maybe (Map a b)
merge mx my = do x <- mx
                 y <- my
                 guard  (compatible x y)
                 return (x `Map.union` y)

compatible x y = allTrue (Map.intersectionWith (==) x y)
        where allTrue = Map.fold (&&) True

crossMatch (a, b) (c, d) = x `mplus` y
        where
           x = match a c `merge` match b d
           y = match a d `merge` match b c


match :: Term -> Term -> Maybe Binding

match (Atom s) t = Just (Map.singleton s t)
match (Not a)      (Not b)      = match a b
match (OfCourse a) (OfCourse b) = match a b
match (WhyNot a)   (WhyNot b)   = match a b

match (a :*: b) (c :*: d) = crossMatch (a, b) (c, d)
match (a :$: b) (c :$: d) = crossMatch (a, b) (c, d)
match (a :&: b) (c :&: d) = crossMatch (a, b) (c, d)
match (a :+: b) (c :+: d) = crossMatch (a, b) (c, d)

match (a :-@: b) (c :-@: d) = match a c `merge` match b d

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
