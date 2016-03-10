module Main(main) where

-- library imports
import System.IO
import System.Environment
import Data.Char
import System.Exit
import FAutomataData
import FAutomataFuncs

main :: IO ()
main = do
	--
	args <- getArgs
	let (simulate, inFile) = procArgs args

-- 	putStrLn $ show (simulate, inFile)
	--
	hInFile <- openFile inFile ReadMode
	fa <- getFiniteAutomata hInFile
-- 	putStrLn $ show fa
	--
	--
	if simulate then transformFiniteAutomata fa
				else dumpFiniteAutomata fa
	--
	hClose hInFile
	return ()

-- parse list of arguments into couple
procArgs :: [String] -> (Bool,String)
procArgs [x,y]
	| x=="-i" = (False, y)
	| x=="-t" = (True, y)
	| otherwise = error "unknown argument"
procArgs _ = error "expects 2 arguments"

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120

