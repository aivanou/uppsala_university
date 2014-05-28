.PHONY: all clean

all: Game.hs tic-tac-logic.hs
	ghc -O2 --make tic-tac-logic.hs

test: Game.hs Test.hs
	ghc -O2 -main-is Test.main --make Test.hs

clean:
	rm -rf *.o *.hi Test tic-tac-logic
