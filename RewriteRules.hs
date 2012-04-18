module RewriteRules (dedual, unexpt) where

import Test.QuickCheck
import Arbitrary

import Syntax
import Parser

import Rewrite

l --@ r = (tt l, tt r)

dedual_rules = [
    "a^^"      --@ "a",
    "(a * b)^" --@ "a^ $ b^",
    "(a $ b)^" --@ "a^ * b^",
    "(a & b)^" --@ "a^ + b^",
    "(a + b)^" --@ "a^ & b^",

    "(a -@ b)^" --@ "a * b^",

    "(!a)^" --@ "?a^",
    "(?a)^" --@ "!a^",

    "1^" --@ "%",
    "0^" --@ "#",
    "#^" --@ "0",
    "%^" --@ "1"
  ]

unexpt_rules = [
    "!!a" --@ "!a",
    "??a" --@ "?a",

    "!?!?a" --@ "!?a",
    "?!?!a" --@ "?!a",

    "!?1" --@ "1",
    "?!%" --@ "%"
  ]

dedual :: Term -> Term
dedual = rewrite' dedual_rules

unexpt :: Term -> Term
unexpt = rewrite' unexpt_rules

simplify :: Term -> Term
simplify = rewrite' (dedual_rules ++ unexpt_rules)

-- Second order idempotence predicate
prop_idempotent :: Eq a => (a -> a) -> a -> Bool
prop_idempotent f x  =  f (f x) == f x

prop_simplify_idempotent x  =  prop_idempotent simplify
prop_dedual_idempotent x    =  prop_idempotent dedual
prop_unexpt_idempotent x    =  prop_idempotent unexpt

prop_dedual_onlyNegativeAtoms x  =  check (dedual x)
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

prop_unexpt_inExponentialLattice1 x  =  lattice (unexpt x)

-- This is a weakened version of the prop above,
-- that makes counter examples easier to find for quickCheck
prop_unexpt_inExponentialLattice2 x  =
  forAllShrink exponentOnlyTerm shrink $ \t -> lattice (unexpt t)

lattice :: Term -> Bool

lattice (OfCourse (WhyNot (OfCourse a))) = latticeNoExp a
lattice (WhyNot   (OfCourse (WhyNot a))) = latticeNoExp a
lattice (OfCourse (WhyNot a))   = latticeNoExp a
lattice (WhyNot   (OfCourse a)) = latticeNoExp a
lattice (OfCourse a) = latticeNoExp a
lattice (WhyNot   a) = latticeNoExp a
lattice (l :*:  r) = lattice l && lattice r
lattice (l :$:  r) = lattice l && lattice r
lattice (l :-@: r) = lattice l && lattice r
lattice (l :&:  r) = lattice l && lattice r
lattice (l :+:  r) = lattice l && lattice r
lattice (Not a)    = lattice a
lattice _ = True

latticeNoExp (OfCourse _) = False
latticeNoExp (WhyNot   _) = False
latticeNoExp t = lattice t

(--@) :: String -> String -> (Term, Term)
dedual_rules :: [Rule]
unexpt_rules :: [Rule]
