module Logic where

import Arbitrary
-- import RewriteRules

import Syntax

simplify :: Term -> Term
-- simplify = unexpt . dedual
simplify = undefined

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

prop_simplify_inExponentialLattice x  =  check (simplify x)
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
