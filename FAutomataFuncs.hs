module FAutomataFuncs(getFiniteAutomata, dumpFiniteAutomata, transformFiniteAutomata) where

import System.IO
import Data.List
import Data.List.Split
import Data.String
import FAutomataData

getFiniteAutomata :: String -> IO FAutomata
getFiniteAutomata content = do
--  putStrLn content
    let lns = lines content
--  putStrLn $ show lns
    let fa = procLns lns
--  putStrLn $ show tsm
    return fa

dumpFiniteAutomata :: FAutomata -> IO ()
dumpFiniteAutomata fa = do
    putStrLn "\ndumping TS ...\n"
    putStrLn $ show fa

transformFiniteAutomata :: FAutomata -> IO ()
transformFiniteAutomata fa = do
    putStrLn "\nsimulating TS ...\n"

procLns :: [String] -> FAutomata
procLns (states:[start]:final:transitions) =
    if null transitions then error "no transitions"
    else FA getStates getAlph (map getRule transitions) [start] getFinal
    where
        getStates = splitOn "," states
        getFinal = splitOn "," final
        getAlph = [] 
        getRule rule = getRule' $ splitOn "," rule
        getRule' :: [String] -> Transition
        getRule' [q1,[sym],q2] =  Trans q1 sym q2
        getRule' [q1,"",q2] =  Trans q1 'E' q2
        getRule' _ = error "bad transition syntax"
procLns _ = error "bad syntax"

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120

