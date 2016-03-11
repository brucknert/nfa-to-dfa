module FAutomataFuncs(getFiniteAutomata, dumpFiniteAutomata, transformFiniteAutomata) where

import System.IO
import Data.List
import Data.List.Split
import Data.String
import Data.Maybe
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
transformFiniteAutomata (FA q a t s f)  = do
    putStrLn "\ntransforming FA ...\n"
    let eps = nub $ getEpsClosure t [s]
    putStrLn $ show eps
    return ()

getAlphabet :: [Transition] -> [ASymbol]
getAlphabet xs = nub $ getAlphabet' xs
    where
        getAlphabet' :: [Transition] -> [ASymbol]
        getAlphabet' (x:xs) = getAlphabetSymbol x : getAlphabet' xs
            where
                getAlphabetSymbol :: Transition -> ASymbol
                getAlphabetSymbol (Trans fs s ts) = s
        getAlphabet' [] = []

getEpsClosure :: [Transition] -> [ASymbol] -> [ASymbol]
getEpsClosure t xs =
  if xs == nxs then xs
    else getEpsClosure' t nxs
  where
    nxs =  getEpsClosure' t xs

getEpsClosure' :: [Transition]  -> [ASymbol] -> [ASymbol]
getEpsClosure' (x:xs) ys = if isNothing ns 
                            then getEpsClosure' xs ys 
                            else getEpsClosure' xs [fromJust ns] ++ ys
    where 
        ns = getEpsCls x ys
        getEpsCls :: Transition -> [ASymbol] -> Maybe ASymbol
        getEpsCls (Trans fs "" ts) xs = if fs `elem` xs then Just ts else Nothing
        getEpsCls _ _ = Nothing
getEpsClosure' [] ys = ys

procLns :: [String] -> FAutomata
procLns (states:[start]:final:transitions) =
    if null transitions then error "no transitions"
    else FA getStates getAlph rules [start] getFinal
    where
        getStates = splitOn "," states
        getFinal = splitOn "," final
        rules = map getRule transitions
        getAlph = delete "" $ getAlphabet rules 
        getRule rule = getRule' $ splitOn "," rule
        getRule' :: [String] -> Transition
        getRule' [q1,sym,q2] =  Trans q1 sym q2
        getRule' _ = error "bad transition syntax"
procLns _ = error "bad syntax"

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120

