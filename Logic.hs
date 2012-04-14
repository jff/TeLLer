module Logic where

import Arbitrary

import Syntax

-- Simplify Negations --

-- Negation Involution
simplify (Not (Not t))    = simplify t

-- DeMorgans Laws
simplify (Not (a :*: b))  = neg a :$:  neg b
simplify (Not (a :$: b))  = neg a :*:  neg b
simplify (Not (a :-@: b)) = neg a :-@: neg b
simplify (Not (a :&: b))  = neg a :+:  neg b
simplify (Not (a :+: b))  = neg a :&:  neg b

-- Exponential Dualities
simplify (Not (OfCourse a)) = simplify (WhyNot   (Not a))
simplify (Not (WhyNot a))   = simplify (OfCourse (Not a))

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

prop_simplify_exponentialLattice x  =  check (simplify x)
  where check (OfCourse (WhyNot (OfCourse a))) = checkNonExp a
        check (WhyNot   (OfCourse (WhyNot a))) = checkNonExp a
        check (OfCourse (WhyNot a))   = checkNonExp a
        check (WhyNot   (OfCourse a)) = checkNonExp a
        check (OfCourse a) = checkNonExp a
        check (WhyNot   a) = checkNonExp a
        check (l :*:  r) = check l && check r
        check (l :$:  r) = check l && check r
        check (l :-@: r) = check l && check r
        check (l :&:  r) = check l && check r
        check (l :+:  r) = check l && check r
        check (Not a)    = check a
        check _ = True

        checkNonExp (OfCourse _) = False
        checkNonExp (WhyNot   _) = False
        checkNonExp t = check t


-- see:
-- http://llwiki.ens-lyon.fr/mediawiki/index.php/Sequent_calculus#One-sided_sequent_calculus
-- http://llwiki.ens-lyon.fr/mediawiki/index.php/Lattice_of_exponential_modalities
