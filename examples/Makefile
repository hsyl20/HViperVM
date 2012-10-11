GHC=ghc -debug -rtsopts -threaded -ldl

all: runtime parser

runtime: RuntimeTest.hs
	${GHC} RuntimeTest.hs

parser: ParseTest.hs
	${GHC} ParseTest.hs

clean:
	rm -f *.hi *.o RuntimeTest ParseTest
