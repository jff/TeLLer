.PHONY: all clean linear-logic

all: linear-logic

linear-logic: Arbitrary.hs
	ghc --make linear-logic.hs

Arbitrary.hs: Syntax.hs
	derive -mArbitrary -iTest.QuickCheck -iSyntax -o $@ $^

clean:
	rm *.o *.hi