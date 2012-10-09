.PHONY: all test clean linear-logic TestRunner

all: linear-logic

linear-logic:
	cabal build

test: TestRunner
	./TestRunner

TestRunner:
	ghc --make -main-is TestRunner TestRunner.hs

clean:
	rm *.o *.hi
