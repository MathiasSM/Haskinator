all: Haskinator clean 

Haskinator : Haskinator.hs
	ghc --make Haskinator

clean:
	rm -f Haskinator.o Oraculo.o Haskinator.hi Oraculo.hi