module Logic where

import Arbitrary

import Syntax

-- Negation Involution
simplify (Not (Not t))    = simplify t

-- DeMorgans Laws
simplify (Not (a :*: b))  = neg a :$:  neg b
simplify (Not (a :$: b))  = neg a :*:  neg b
simplify (Not (a :-@: b)) = neg a :-@: neg b
simplify (Not (a :&: b))  = neg a :+:  neg b
simplify (Not (a :+: b))  = neg a :&:  neg b

-- Exponential Dualities
simplify (Not (OfCourse a)) = WhyNot   (neg a)
simplify (Not (WhyNot a))   = OfCourse (neg a)

-- Unit Dualities
simplify (Not One)    = Bottom
simplify (Not Zero)   = Top
simplify (Not Bottom) = One
simplify (Not Top)    = Zero

-- Passthrough
simplify (a :*: b)  = simplify a :*:  simplify b
simplify (a :$: b)  = simplify a :$:  simplify b
simplify (a :-@: b) = simplify a :-@: simplify b
simplify (a :&: b)  = simplify a :&:  simplify b
simplify (a :+: b)  = simplify a :+:  simplify b

simplify (OfCourse t) = OfCourse (simplify t)
simplify (WhyNot t)   = WhyNot   (simplify t)

simplify t = t


neg t = simplify (Not t)


prop_simplify_idempotent x  =  simplify (simplify x) == simplify x
prop_simplify_onlyNegativeAtoms x  =  check (simplify x)
  where check (Not (Atom _)) = True
        check (Not _)    = False
        check (l :*:  r) = check l && check r
        check (l :$:  r) = check l && check r
        check (l :-@: r) = check l && check r
        check (l :&:  r) = check l && check r
        check (l :+:  r) = check l && check r
        check (OfCourse a) = check a
        check (WhyNot a)   = check a
        check _ = True
