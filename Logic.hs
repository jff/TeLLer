module Logic where

import Arbitrary

import Syntax

-- Negation Involution
pushNot (Not (Not t))    = pushNot t

-- DeMorgans Laws
pushNot (Not (a :*: b))  = neg a :$:  neg b
pushNot (Not (a :$: b))  = neg a :*:  neg b
pushNot (Not (a :-@: b)) = neg a :-@: neg b
pushNot (Not (a :&: b))  = neg a :+:  neg b
pushNot (Not (a :+: b))  = neg a :&:  neg b

-- Exponential Dualities
pushNot (Not (OfCourse a)) = WhyNot   (neg a)
pushNot (Not (WhyNot a))   = OfCourse (neg a)

-- Unit Dualities
pushNot (Not One)    = Bottom
pushNot (Not Zero)   = Top
pushNot (Not Bottom) = One
pushNot (Not Top)    = Zero

-- Passthrough
pushNot (a :*: b)  = pushNot a :*:  pushNot b
pushNot (a :$: b)  = pushNot a :$:  pushNot b
pushNot (a :-@: b) = pushNot a :-@: pushNot b
pushNot (a :&: b)  = pushNot a :&:  pushNot b
pushNot (a :+: b)  = pushNot a :+:  pushNot b

pushNot (OfCourse t) = OfCourse (pushNot t)
pushNot (WhyNot t)   = WhyNot   (pushNot t)

pushNot t = t


neg t = pushNot (Not t)


prop_pushNot_idempotent x  =  pushNot (pushNot x) == pushNot x
prop_pushNot_onlyNegativeAtoms x  =  check (pushNot x)
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
