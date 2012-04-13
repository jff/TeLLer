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

instance Arbitrary Term where
  arbitrary = sized $ \n -> genTerm n

  shrink (a :*:  b) = [a, b]
  shrink (a :$:  b) = [a, b]
  shrink (a :-@: b) = [a, b]
  shrink (a :&:  b) = [a, b]
  shrink (a :+:  b) = [a, b]

  shrink (Not      a) = [a]
  shrink (WhyNot   a) = [a]
  shrink (OfCourse a) = [a]

  shrink _ = []

as :: Gen a -> (a -> b) -> Gen b
as = flip fmap
