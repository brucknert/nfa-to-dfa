{- |
Module      :  FAutomataFuncs
Description :  Functions.
Copyright   :  (c) Tomas Bruckner 2016
License     :  MIT

Maintainer  :  xbruck02@stud.fit.vutbr.cz
Stability   :  stable
Portability :  portable

Functions.
-}

module FAutomataFuncs(
  getFiniteAutomata
  , dumpFiniteAutomata
  , transformFiniteAutomata
) where

import System.IO
import Data.List
import Data.List.Split
import Data.Maybe
import FAutomataData

-- | Parses program's input Finite Automata 'FAutomata'.
getFiniteAutomata :: String         -- ^ Program's input content
                  -> IO FAutomata   -- ^ Return Finite Automata 'FAutomata'
getFiniteAutomata content = do
    let lns = lines content
    let fa = procLns lns
    return fa

-- | Prints program's input Finite Automata 'FAutomata' in desirable format.
dumpFiniteAutomata  :: FAutomata    -- ^ Program's input Finite Automata 'FAutomata'
                    -> IO ()        -- ^ Output Finite Automata
dumpFiniteAutomata fa = do
    putStrLn $ show fa

-- | Transforms Finite Automata 'FAutomata' to Deterministic Finite Automata 'DFAutomata' and prints it.
transformFiniteAutomata :: FAutomata -> IO ()
transformFiniteAutomata fa@(FA q a t s f)  = do
    let q0 = getInitialState t s
    let dfa = DFA [q0] a [] q0 []
    let tdfa = getDFAutomata dfa fa [q0]
    putStrLn $ show tdfa
    return ()

getDFAutomata :: DFAutomata -> FAutomata -> [EpsClosure] -> DFAutomata
getDFAutomata dfs@(DFA ds da dt dst de) fa@(FA s a t st e) (x:xs) =
  if null newstates
    then getDFAutomata (DFA ds da (dt ++ dtrans) dst (getFiniteStates ds e)) fa xs
    else getDFAutomata (DFA (ds ++ newstates) da (dt ++ dtrans) dst de) fa (xs ++ newstates)
      where
        dtrans = getNewTransition ds fa x (getHighestNum ds 0)
        newstates = getNewEps ds dtrans
getDFAutomata dfs _ [] = dfs

getHighestNum :: [EpsClosure] -> Int -> Int
getHighestNum (x:xs) num =
  if num > n
    then getHighestNum xs num
    else getHighestNum xs n
    where
      n = getHighestNum' x
getHighestNum [] num = num + 1

getHighestNum' :: EpsClosure -> Int
getHighestNum' (ECls n _) = n

getFiniteStates :: [EpsClosure] -> [AState] -> [EpsClosure]
getFiniteStates (x:xs) ss =
  if isFiniteState x ss
    then [x] ++ getFiniteStates xs ss
    else getFiniteStates xs ss
getFiniteStates [] _ = []

-- vsechny stavy
getNewEps :: [EpsClosure] -> [DTransition] -> [EpsClosure]
getNewEps e (x:xs) = (getNewEps' e x False) ++ (getNewEps e xs)
getNewEps _ [] = []

-- vsechny stavy
getNewEps' :: [EpsClosure] -> DTransition -> Bool -> [EpsClosure]
getNewEps' (x:xs) t False = getNewEps' xs t (getNewEps'' x t)
getNewEps' _ _ True = []
getNewEps' [] (DTrans fs s ts) False = [ts]

-- jeden stav
getNewEps'' :: EpsClosure -> DTransition -> Bool
getNewEps'' (ECls _ ns) (DTrans _ _ (ECls _ dns)) =
  if ns == dns then True
    else False

getAlphabet :: [Transition] -> [ASymbol]
getAlphabet xs = nub $ getAlphabet' xs
    where
        getAlphabet' :: [Transition] -> [ASymbol]
        getAlphabet' (x:xs) = getAlphabetSymbol x : getAlphabet' xs
            where
                getAlphabetSymbol :: Transition -> ASymbol
                getAlphabetSymbol (Trans fs s ts) = s
        getAlphabet' [] = []

isFiniteState :: EpsClosure -> [AState] -> Bool
isFiniteState (ECls s os) (x:xs) = if x `elem` os then True else isFiniteState (ECls s os) xs
isFiniteState _ [] = False

-- vsechny symboly
getNewTransition :: [EpsClosure] -> FAutomata -> EpsClosure -> Int -> [DTransition]
getNewTransition ds (FA q (a:as) t s f) e num = if null nts
    then (getNewTransition ds (FA q (as) t s f) e num)
    else [(DTrans e a xx)]  ++ (getNewTransition add (FA q (as) t s f) e (getHighestNum add 1))
    where
        nts = (getNewTransition' t a e)
        xx = createNewEpsClosure ds num $ sort (getEpsClosure t nts)
        add = addEpsClosure ds xx
getNewTransition _ (FA _ [] _ _ _) _ _ = []

addEpsClosure :: [EpsClosure] -> EpsClosure -> [EpsClosure]
addEpsClosure [] eps = [eps]
addEpsClosure es eps = es ++ (addEpsClosure' es eps)
  where
    addEpsClosure' :: [EpsClosure] -> EpsClosure -> [EpsClosure]
    addEpsClosure' (x:xs) eps@(ECls _ e) = if isNewEpsClosure x e
      then  addEpsClosure' xs eps
      else []
    addEpsClosure' [] eps = [eps]

createNewEpsClosure :: [EpsClosure] -> Int -> [AState] -> EpsClosure
createNewEpsClosure [] num ss = (ECls num ss)
createNewEpsClosure (x:xs) num ss = if isNewEpsClosure x ss
  then createNewEpsClosure xs num ss
  else x

isNewEpsClosure :: EpsClosure -> [AState] -> Bool
isNewEpsClosure (ECls _ ss) as = (sort ss) /= (sort as)

-- vsechny pravidla
getNewTransition' :: [Transition] -> ASymbol -> EpsClosure -> [AState]
getNewTransition' (x:xs) a (ECls s os) =
    if isNothing tns then ntns
        else [fromJust tns] ++ ntns
    where
        tns = getNewTransition'' x a os
        ntns = getNewTransition' xs a (ECls s os)
getNewTransition' [] x y = []

-- jedno pravidlo, kouknu jestli neco v uzaveru neni na fromState a pres symbol a do toState
getNewTransition'' :: Transition -> ASymbol -> [AState] -> Maybe AState
getNewTransition'' (Trans fs s ts) a xs = if fs `elem` xs && s == a then Just ts else Nothing

-- Vytvorit epsilonovy uzaver k stavu
getInitialState :: [Transition] -> AState -> EpsClosure
getInitialState t s = ECls 1 (sort (getEpsClosure t [s]))

getEpsClosure :: [Transition] -> [AState] -> [AState]
getEpsClosure t xs =
  if xs == nxs then nub xs
    else nub $ getEpsClosure' t nxs
  where
    nxs =  getEpsClosure' t xs

getEpsClosure' :: [Transition]  -> [AState] -> [ASymbol]
getEpsClosure' (x:xs) ys = if isNothing ns
                            then getEpsClosure' xs ys
                            else getEpsClosure' xs ([fromJust ns] ++ ys)
    where
        ns = getEpsCls x ys
        getEpsCls :: Transition -> [AState] -> Maybe ASymbol
        getEpsCls (Trans fs "" ts) xs = if fs `elem` xs then Just ts else Nothing
        getEpsCls _ _ = Nothing
getEpsClosure' [] ys = sort ys

procLns :: [String] -> FAutomata
procLns (states:[start]:final:transitions) =
    if null transitions then error "no transitions"
    else FA getStates getAlph rules [start] getFinal
    where
        getStates = splitOn "," states
        getFinal = splitOn "," final
        rules = map getRule transitions
        getAlph = delete "" $ sort (getAlphabet rules)
        getRule rule = getRule' $ splitOn "," rule
        getRule' :: [String] -> Transition
        getRule' [q1,sym,q2] =  Trans q1 sym q2
        getRule' _ = error "bad transition syntax"
procLns _ = error "bad syntax"

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120
