module Syntax where

data Term 
     = Atom String
     | Not Term

     -- Multiplicatives
     | Term :*: Term  -- Conjunction
     | Term :$: Term  -- Disjunction
     | Term :-@: Term -- Linear Implication

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



showPrecOp prec op d (l, r) = showParen (d > prec) $
      showsPrec (prec+1) l . showString op . showsPrec (prec+1) r

instance Show Term where
  showsPrec d (Atom s) = showString s

  showsPrec d (Not t)  = showParen (d > prec) $
      showsPrec (prec+1) t . showString "^"
    where prec = 8

  showsPrec d (OfCourse t)  = showParen (d > prec) $
      showString "!" . showsPrec (prec+1) t
    where prec = 7

  showsPrec d (WhyNot t)  = showParen (d > prec) $
      showString "?" . showsPrec (prec+1) t
    where prec = 7

  showsPrec d (l :*: r)  = showPrecOp 6 "*"  d (l, r)
  showsPrec d (l :$: r)  = showPrecOp 6 "$"  d (l, r)
  showsPrec d (l :-@: r) = showPrecOp 2 "  -@  " d (l, r)

  showsPrec d (l :&: r) = showPrecOp 4 " & " d (l, r)
  showsPrec d (l :+: r) = showPrecOp 4 " + " d (l, r)

  showList []     = id
  showList (x:[]) = shows x . showString "."
  showList (x:xs) = shows x . showString ".\n" . showList xs
