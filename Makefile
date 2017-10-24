all: Main clean 

Main : Main.hs
	ghc --make Main

clean:
	rm -f Main.o Oraculo.o Main.hi Oraculo.hi