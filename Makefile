default:
	ghc --make FAutomataMain.hs

run:
	ghc --make FAutomataMain.hs && ./FAutomataMain -t example/test.in

runstdin:
	ghc --make FAutomataMain.hs && ./FAutomataMain -t

doc:
	haddock -h -o doc FAutomataData.hs FAutomataFuncs.hs FAutomataMain.hs

clean:
	rm *.o *.hi FAutomataMain
