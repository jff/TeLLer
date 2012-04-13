.PHONY: all clean linear-logic

all: linear-logic

linear-logic: Arbitrary.hs
	ghc --make linear-logic.hs

clean:
	rm *.o *.hi