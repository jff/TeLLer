module Printer where

import Syntax

showsTermOp  prec op d (l, r) = showParen (d > prec) $
      showsTerm (prec+1) l . showString op . showsTerm prec r

showTerm :: Term -> String
showTerm term = showsTerm 0 term ""

showsTerm :: Int -> Term -> ShowS
showsTerm d (Atom s) = showString s

showsTerm d (Not t)  = showParen (d > prec) $
    showsTerm prec t . showString "^"
  where prec = 8

showsTerm d (OfCourse t)  = showParen (d > prec) $
    showString "!" . showsTerm prec t
  where prec = 7

showsTerm d (WhyNot t)  = showParen (d > prec) $
    showString "?" . showsTerm prec t
  where prec = 7


showsTerm d (l :*:  r) = showsTermOp 6 "*"  d (l, r)
showsTerm d (l :$:  r) = showsTermOp 6 "$"  d (l, r)
showsTerm d (l :-@: r) = showsTermOp 2 "  -@  " d (l, r)

showsTerm d (l :&: r) = showsTermOp 4 " & " d (l, r)
showsTerm d (l :+: r) = showsTermOp 4 " + " d (l, r)


showsTerm d Top    = showString "#"
showsTerm d Bottom = showString "%"
showsTerm d One    = showString "1"
showsTerm d Zero   = showString "0"


showTerms [] = "(empty)"
showTerms ls = showsTerms ls ""

showsTerms []     = id
showsTerms (x:[]) = showsTerm 0 x . showString "."
showsTerms (x:xs) = showsTerm 0 x . showString ".\n" . showsTerms xs
