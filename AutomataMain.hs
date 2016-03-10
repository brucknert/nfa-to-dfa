-- 1. Q':= 2^Q \ {∅}.
-- 2. q'0 := ε-uzávěr(q0)
-- 3. δ': Q' × Σ → Q' je vypočtena takto:
-- • Nechť ∀T ∈ Q', a ∈ Σ : δ_(T, a) = Uq∈T δ(q, a).
-- • Pak pro každé T ∈ Q', a ∈ Σ:
-- (a) pokud δ_(T, a) != ∅, pak δ'(T, a) = ε-uzávěr(δ_(T, a)),
-- (b) jinak δ'(T, a) není definována.
-- 4. F' := {S | S ∈ Q' ∧ S ∩ F != ∅}.
--
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
