module Main(main) where

import System.IO
import System.Environment
import Data.Char
import System.Exit

import AutomataData
import AutomataData

main :: IO ()
main = do
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

procArgs :: [String] -> (Bool,String)
procArgs [x,y]
	| x=="-i" = (False, y)
	| x=="-t" = (True, y)
	| otherwise = error "unknown argument"
procArgs _ = error "expects 2 arguments"

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120

