module TuringFuncs
( getTuringMachine
, simulateTuringMachine
, dumpTuringMachine
) where

-- library imports
import System.IO
import Data.List
import Data.List.Split
import Data.String

-- our imports
import TuringData

--
getTuringMachine :: Handle -> IO TSMachine
getTuringMachine hIn = do
	content <- hGetContents hIn
-- 	putStrLn content
	let lns = lines content
-- 	putStrLn $ show lns
	let tsm = procLns lns
-- 	putStrLn $ show tsm
	return tsm

-- process input lines by dividing them into 4 parts
procLns :: [String] -> TSMachine
procLns (states:start:final:transitions) =
	if null transitions then error "no transitions"
						else TSM getStates getAlph (map getRule transitions) start final
	where
		getStates = splitOn "," states
		getAlph = "we do not reallt care for alphabet"
		getRule rule = getRule' $ splitOn "," rule
		getRule' :: [String] -> Transition
		getRule' [q1,[sym],q2,action] = Trans q1 sym q2 (getAction action)
		getRule' _ = error "bad transition syntax"
		getAction ['<'] = ALeft
		getAction ['>'] = ARight
		getAction [c] = AWrite c
		getAction _ = error "bad action"
procLns _ = error "bad syntax"

-- dump TS to stdout if option '-i' selected
-- !!! we use show function for TS, but it does not produce prescribed format
-- !!! you can either make it behave as expected and use it directly (no need for 'dumpTuringMachine')
-- !!! or ixmplement your own show function with pretty output (good for debugging)
-- !!! and define separate function like this to dump object in expected format (it is not very pretty -> not good for debugging)
dumpTuringMachine :: TSMachine -> IO ()
dumpTuringMachine ts = do
	putStrLn "\ndumping TS ...\n"
	putStrLn $ show ts

-- simulate TS on input
simulateTuringMachine :: TSMachine -> TInput -> IO ()
simulateTuringMachine ts input = do
	putStrLn "\nsimulating TS ...\n"
	runUTM (trans ts) (start ts) (end ts) (Tape '$' [] input)  -- rules expect $ as the first tape symbol

-- run TM in UTM
runUTM :: [Transition] -> TState -> TState -> Tape TSymbol -> IO ()
runUTM rules start stop tape@(Tape x lts rts)
	| start == stop = do
		printTMconfig start tape
		putStrLn "\n===> ACCEPT\n"
	| otherwise = do
		printTMconfig start tape
		let apply = findRule rules start x
		putStrLn $ (show apply) ++ "\n"
		runUTM rules (toState apply) stop (step (toAction apply) tape)

-- find rule for state and symbol
findRule :: [Transition] -> TState -> TSymbol -> Transition
findRule [] _ _ = error "no rule found"
findRule (r:rs) q x = if (fromState r) == q && (fromSym r) == x then r else findRule rs q x

-- do one step on tape
step :: Action -> Tape TSymbol -> Tape TSymbol
step (ALeft)    (Tape _ [] _)      = error "can not move left"
step (ALeft)    (Tape x (l:ls) rs) = Tape l ls (x:rs)
step (ARight)   (Tape x ls [])     = Tape '$' (x:ls) []
step (ARight)   (Tape x ls (r:rs)) = Tape r (x:ls) rs
step (AWrite w) (Tape _ ls rs)     = Tape w ls rs

-- debug function for TS config printing
printTMconfig :: TState -> Tape TSymbol -> IO ()
printTMconfig q tape = do putStrLn $ "(" ++ q ++ "," ++ (show tape) ++ ")"
