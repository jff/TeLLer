module Arbitrary where
import Test.QuickCheck
import Syntax

identifier = listOf1 $ elements ['a'..'z']

genTerm 0 = frequency [
    (5, identifier `as` Atom),
    (1, elements [Top, Bottom, One, Zero])
  ]

genTerm n | n > 0 = oneof [
    genTerm 0, 
    genBinaryTerm (n `div` 2),
    genUnaryTerm  (n `div` 2)
  ]

genBinaryTerm n = do
  connective <- elements [(:*:), (:$:), (:-@:), (:&:), (:+:)]
  l <- genTerm n
  r <- genTerm n
  return (l `connective` r)

genUnaryTerm n = do
  connective <- elements [Not, OfCourse, WhyNot]
  u <- genTerm n
  return (connective u)

-- This make take a very long time for large ASTs
shrinkDeep op (a, b) = [a, b] ++ [ l `op` r | l <- shrink a, r <- shrink b]
-- shrinkDeep op (a, b) = [a, b]

instance Arbitrary Term where
  arbitrary = sized $ \n -> genTerm n

  shrink (a :*:  b) = shrinkDeep (:*:) (a, b)
  shrink (a :$:  b) = shrinkDeep (:$:) (a, b)
  shrink (a :-@: b) = shrinkDeep (:-@:) (a, b)
  shrink (a :&:  b) = shrinkDeep (:&:) (a, b)
  shrink (a :+:  b) = shrinkDeep (:+:) (a, b)

  shrink (Atom s) = [Atom a | a <- shrink s, not (null a)]

  shrink (Not  t)     = [Not       t' | t' <- shrink t]
  shrink (OfCourse t) = [OfCourse  t' | t' <- shrink t]
  shrink (WhyNot   t) = [WhyNot    t' | t' <- shrink t]

  shrink _ = []

as :: Gen a -> (a -> b) -> Gen b
as = flip fmap
