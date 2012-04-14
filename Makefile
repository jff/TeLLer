.PHONY: all test clean linear-logic TestRunner

all: linear-logic

linear-logic: Arbitrary.hs
	ghc --make linear-logic.hs

test: TestRunner
	./TestRunner

TestRunner:
	ghc --make -main-is TestRunner TestRunner.hs

clean:
	rm *.o *.hi
