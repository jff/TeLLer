module RewriteRules where

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

dedual :: Term -> Term
dedual t = rewrite' dedual_rules t

prop_dedual_idempotent x  =  dedual (dedual x) == dedual x

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
