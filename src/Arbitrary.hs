module Arbitrary where
import Test.QuickCheck
import Syntax

identifier :: Gen String
identifier = elements $ zipWith (\c n -> c:show n) ['a'..'z'] [0..]

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
  --connective <- elements [(:*:), (:$:), (:-@:), (:&:), (:+:)]
  connective <- elements [(:*:), (:$:), (:&:), (:+:)]
  l <- genTerm n
  r <- genTerm n
  return (l `connective` r)

genUnaryTerm n = do
  connective <- elements [Not, OfCourse, WhyNot]
  u <- genTerm n
  return (connective u)

exponentOnlyTerm :: Gen Term
exponentOnlyTerm = sized $ \n -> do
  connectives <- listOf $ elements [OfCourse, WhyNot]
  let term = foldr ($) (Atom "a") connectives
  return term


-- This make take a very long time for large ASTs
shrinkDeep op (a, b) = [a, b] ++ [ l `op` r | l <- shrink' a, r <- shrink' b]
           where shrink' x = take 2 (shrink x)
-- shrinkDeep op (a, b) = [a, b]

instance Arbitrary Term where
  arbitrary = sized $ \n -> genTerm n

  shrink (a :*:  b) = shrinkDeep (:*:)  (a, b)
  shrink (a :$:  b) = shrinkDeep (:$:)  (a, b)
  --shrink (a :-@: b) = shrinkDeep (:-@:) (a, b)
  --shrink ((:-@:) a b _) = shrinkDeep (:-@:) (a, b)
  shrink (a :&:  b) = shrinkDeep (:&:)  (a, b)
  shrink (a :+:  b) = shrinkDeep (:+:)  (a, b)

  shrink (Atom s) = []

  shrink (Not  t)     = t : [Not       t' | t' <- shrink t]
  shrink (OfCourse t) = t : [OfCourse  t' | t' <- shrink t]
  shrink (WhyNot   t) = t : [WhyNot    t' | t' <- shrink t]

  shrink _ = []

as :: Gen a -> (a -> b) -> Gen b
as = flip fmap
