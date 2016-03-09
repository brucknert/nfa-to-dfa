module Main(main) where

-- library imports
import System.IO
import System.Environment
import Data.Char
import System.Exit

-- our imports
import TuringData
import TuringFuncs

-- program's entry point
main :: IO ()
main = do
	--
	args <- getArgs
	let (simulate, inFile) = procArgs args
-- 	putStrLn $ show (simulate, inFile)
	--
	hInFile <- openFile inFile ReadMode
	ts <- getTuringMachine hInFile
-- 	putStrLn $ show ts
	--
	input' <- hGetContents stdin
	let input = filter (/= '\n') input'
-- 	putStrLn input
	--
	if simulate then simulateTuringMachine ts input
				else dumpTuringMachine ts
	--
	hClose hInFile
	return ()

-- parse list of arguments into couple
procArgs :: [String] -> (Bool,String)
procArgs [] = error "expects 2 arguments"  -- not really needed in this program but you can do something similar,
procArgs [x] = error "expects 2 arguments" -- if you also want to handle zero or one arguments.
procArgs [x,y]
	| x=="-i" = (False, y)
	| x=="-s" = (True, y)
	| otherwise = error "unknown argument"
procArgs _ = error "expects 2 arguments"
