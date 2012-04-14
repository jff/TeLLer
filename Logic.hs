module Logic where

import Arbitrary

import Syntax

simplify :: Term -> Term
simplify = unexpt . dedual


-- Normalize Dualities --

-- Negation Involution
dedual (Not (Not t))    = dedual t

-- DeMorgans Laws
dedual (Not (a :*: b))  = neg a :$:  neg b
dedual (Not (a :$: b))  = neg a :*:  neg b
dedual (Not (a :-@: b)) = neg a :-@: neg b
dedual (Not (a :&: b))  = neg a :+:  neg b
dedual (Not (a :+: b))  = neg a :&:  neg b

-- Exponential Dualities
dedual (Not (OfCourse a)) = dedual (WhyNot   (Not a))
dedual (Not (WhyNot a))   = dedual (OfCourse (Not a))

-- Unit Dualities
dedual (Not One)    = Bottom
dedual (Not Zero)   = Top
dedual (Not Bottom) = One
dedual (Not Top)    = Zero

-- Passthrough
dedual (a :*: b)  = dedual a :*:  dedual b
dedual (a :$: b)  = dedual a :$:  dedual b
dedual (a :-@: b) = dedual a :-@: dedual b
dedual (a :&: b)  = dedual a :&:  dedual b
dedual (a :+: b)  = dedual a :+:  dedual b

dedual (OfCourse t) = OfCourse (dedual t)
dedual (WhyNot t)   = WhyNot   (dedual t)

dedual t = t

neg t = dedual (Not t)


-- Exponential Lattice --

-- Digging
unexpt (OfCourse (OfCourse a)) = unexpt (OfCourse a)
unexpt (WhyNot   (WhyNot   a)) = unexpt (WhyNot   a)

-- Reduction
unexpt (OfCourse (WhyNot (OfCourse (WhyNot a)))) = unexpt (OfCourse (WhyNot a))
unexpt (WhyNot (OfCourse (WhyNot (OfCourse a)))) = unexpt (WhyNot (OfCourse a))

-- Action on Units
unexpt (OfCourse (WhyNot One))      = One
unexpt (WhyNot   (OfCourse Bottom)) = Bottom

-- Passthrough
unexpt (a :*: b)  = unexpt a :*:  unexpt b
unexpt (a :$: b)  = unexpt a :$:  unexpt b
unexpt (a :-@: b) = unexpt a :-@: unexpt b
unexpt (a :&: b)  = unexpt a :&:  unexpt b
unexpt (a :+: b)  = unexpt a :+:  unexpt b

unexpt (Not a)      = Not (unexpt a)
unexpt (OfCourse t) = OfCourse (unexpt t)
unexpt (WhyNot t)   = WhyNot   (unexpt t)

unexpt t = t

-- Props --

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
