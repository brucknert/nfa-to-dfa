module FAutomataFuncs(getFiniteAutomata, dumpFiniteAutomata, transformFiniteAutomata) where

import System.IO
import Data.List
import Data.List.Split
import Data.String
import FAutomataData

getFiniteAutomata :: String -> IO FAutomata
getFiniteAutomata content = do
    let lns = lines content
    let fa = procLns lns
    return fa

dumpFiniteAutomata :: FAutomata -> IO ()
dumpFiniteAutomata fa = do
    putStrLn $ show fa

transformFiniteAutomata :: FAutomata -> IO ()
transformFiniteAutomata fa = do
    putStrLn "\ntransforming FA ...\n"

getAlphabet :: [Transition] -> [ASymbol]
getAlphabet xs = nub . getAlphabet' xs

getAlphabet' :: [Transition] -> [ASymbol]
getAlphabet' (x:xs) = getAlphabetSymbol x : getAlphabet xs
getAlphabet' [] = []

getEpsClosure :: [Transition] -> [ASymbol] -> [ASymbol]
getEpsClosure t xs =
  if xs == nxs then xs
    else getEpsClosure' t nxs
  where
    nxs =  getEpsClosure' t xs

getEpsClosure' :: [Transition] -> [ASymbol] -> [ASymbol]
getEpsClosure' (x:xs) ys = []


getAlphabetSymbol :: Transition -> ASymbol
getAlphabetSymbol (Trans fs s ts) xs = s

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
        getRule' [q1,sym,q2] =  Trans q1 sym q2
        getRule' _ = error "bad transition syntax"
procLns _ = error "bad syntax"

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120
