module Syntax where

data Term 
     = Atom String
     | Not Term

     -- Multiplicatives
     | Term :*: Term  -- Conjunction
     | Term :$: Term  -- Disjunction
     | (:-@:) Term Term (Maybe String) -- Linear Implication. Actions have a name (String) that is used in the causality graph.

     -- Additives
     | Term :&: Term  -- Conjunciton
     | Term :+: Term  -- Disjunction

     -- Exponentials
     | OfCourse Term
     | WhyNot   Term

     -- Units
     | Top
     | Bottom
     | One
     | Zero

  deriving (Show, Eq, Ord)

atomic :: Term -> Bool
atomic (Atom _) = True
atomic t = unitary t

unitary :: Term -> Bool
unitary Top      = True
unitary Bottom   = True
unitary One      = True
unitary Zero     = True
unitary _        = False
