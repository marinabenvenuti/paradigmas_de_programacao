SOURCES = Board.hs Solver.hs main.hs

main: $(SOURCES)
	ghc --make main.hs -Wall -outputdir build/

loud: $(SOURCES)
	ghc --make main.hs -Weverything -outputdir build/

clean:
	rm -rf build/ *.o *.hi main