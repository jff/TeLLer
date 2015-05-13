module Printer where

import Syntax

showsTermOp  prec op d (l, r) = showParen (d > prec) $
      showsTerm (prec+1) l . showString op . showsTerm prec r

-- JFF: In order to minimize the changes I have to make from Andy's code,
-- I'm cheating a bit...
showTerm :: Term -> String
showTerm ((:-@:) l r (Just desc)) = (showsTerm 0 ((:-@:) l r Nothing) "") ++ "\t(" ++ desc ++ ")"
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
--showsTerm d (l :-@: r) = showsTermOp 2 "  -@  " d (l, r)
-- It doesn't matter what the description is, because that is considered in showTerm
showsTerm d ((:-@:) l r _) = showsTermOp 2 "  -@  " d (l, r)

showsTerm d (l :&: r) = showsTermOp 4 " & " d (l, r)
showsTerm d (l :+: r) = showsTermOp 4 " + " d (l, r)


showsTerm d Top    = showString "#"
showsTerm d Bottom = showString "%"
showsTerm d One    = showString "1"
showsTerm d Zero   = showString "0"


showTerms [] = "(empty)"
--showTerms ls = showsTerms ls ""
showTerms (x:[]) = showTerm x ++ "."
showTerms (x:xs) = showTerm x ++ ".\n" ++ showTerms xs

showsTerms []     = id
showsTerms (x:[]) = showsTerm 0 x . showString "."
showsTerms (x:xs) = showsTerm 0 x . showString ".\n" . showsTerms xs



-- showAction is defined only on actions
showAction ((:-@:) l r (Just desc)) = desc
showAction ((:-@:) l r Nothing) = showTerm $ ((:-@:) l r Nothing)
