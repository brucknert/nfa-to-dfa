{- |
Module      :  FAutomataFuncs
Description :  Functions.
Copyright   :  (c) Tomas Bruckner 2016
License     :  MIT

Maintainer  :  xbruck02@stud.fit.vutbr.cz
Stability   :  stable
Portability :  portable

More info about transformation algorithm is in the README.
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

-- | Parses program's input FA 'FAutomata'.
getFiniteAutomata :: String         -- ^ Program's input content
                  -> IO FAutomata   -- ^ Return FA 'FAutomata'
getFiniteAutomata content = do
    let lns = lines content
    let fa = procLns lns
    return fa

-- | Parses program input and returns FA 'FAutomata'.
procLns :: [String]     -- ^ Lines of program's input
        -> FAutomata    -- ^ Parsed FA 'FAutomata'
procLns (states:[initial]:final:transitions) =
    if null transitions then error "no transitions"
    else FA getStates getAlph rules [initial] getFinal
    where
        getStates = splitOn "," states
        getFinal = splitOn "," final
        rules = map getRule transitions
        getAlph = getAlphabet rules
        getRule rule = getRule' $ splitOn "," rule
        getRule' :: [String] -> Transition
        getRule' [q1,sym,q2] =  Trans q1 sym q2
        getRule' x = error "bad transition syntax"
procLns _ = error "bad syntax"

-- | Parses FA's alphabet from array of 'Transition'.
getAlphabet :: [Transition]   -- ^ Transitions of FA 'FAutomata'
            -> [ASymbol]      -- ^ FA's alphabet
getAlphabet xs = sort $ delete "" $ nub $ getAlphabet' xs
    where
        getAlphabet' :: [Transition] -> [ASymbol]
        getAlphabet' (x:xs) = sym x : getAlphabet' xs
        getAlphabet' [] = []

-- | Prints program's input FA 'FAutomata' in desirable format.
dumpFiniteAutomata  :: FAutomata    -- ^ Program's input FA 'FAutomata'
                    -> IO ()        -- ^ Output FA
dumpFiniteAutomata fa = do
    putStrLn $ show fa

-- | Transforms FA 'FAutomata' to DFA 'DFAutomata' and prints it.
transformFiniteAutomata :: FAutomata  -- ^ FA to be trasnformed
                        -> IO ()      -- ^ Output DFA
transformFiniteAutomata fa@(FA q a t s f)  = do
    let q0 = getInitialState t s
    let dfa = DFA [q0] a [] q0 []
    let tdfa = getDFAutomata dfa fa [q0]
    putStrLn $ show tdfa

{-|
Transforms empty DFA 'DFAutomata' from FA 'FAutomata'.
Returns transformed transformed DFA 'DFAutomata'.
Function takes array of unprocessed states and generates new transitions for DFA.
If a new transition is to a new state (not yet in DFA), than this state is add to the states
of the DFA and to the array of unprocessed states of DFA.
Finishes when there are no longer any unprocessed states of DFA (every transition is generated).
-}
getDFAutomata :: DFAutomata     -- ^ DFA to process
              -> FAutomata      -- ^ Original FA
              -> [EpsClosure]   -- ^ Unprocessed states of DFA
              -> DFAutomata     -- ^ Processed DFA
getDFAutomata dfa@(DFA ds da dt dst de) fa (x:xs) =
    if null newstates
    then getDFAutomata (DFA ds da (dt ++ dtrans) dst (getFiniteStates ds $ finals fa)) fa xs
    else getDFAutomata (DFA (ds ++ newstates) da (dt ++ dtrans) dst de) fa (xs ++ newstates)
    where
      dtrans = getNewDTransitions ds (trans fa) (alphabet fa) x (getNextIndex ds 0)
      newstates = getNewEClsFromDTrans ds dtrans
getDFAutomata dfa _ [] = dfa

-- | Parses initial state for DFA 'DFAutomata' from array of 'Transition'.
getInitialState :: [Transition]   -- ^ Transitions of FA 'FAutomata'
                -> AState         -- ^ Initial state of FA 'FAutomata'
                -> EpsClosure     -- ^ Initial state of DFA 'DFAutomata'
getInitialState t s = ECls 1 $ sort $ getOrigStates t [s]

{-|
Creates new transitions 'DTransition' from specified state 'EpsClosure' for every symbol
from Deterministic Finite Automat's alphabet. If dtoState of transition is new, it creates
new 'EpsClosure'.
-}
getNewDTransitions :: [EpsClosure]    -- ^ All states of DFA
                -> [Transition]       -- ^ All transitions of FA 'FAutomata'
                -> [ASymbol]          -- ^ Alphabet symbols of DFA to process
                -> EpsClosure         -- ^ fromState in 'DTransition'
                -> Int                -- ^ Next free index for new state of DFA
                -> [DTransition]      -- ^ New transitions for DFA
getNewDTransitions ds t (a:as) eps num =
    if null nts
    then getNewDTransitions ds t (as) eps num
    else dtrans : nextDTrans
    where
        nts = getNewDTransitions' t a eps
        newState = createEpsClosure ds num $ sort (getOrigStates t nts)
        newECls = nub $ newState : ds
        dtrans = DTrans eps a newState
        nextDTrans = getNewDTransitions newECls t (as) eps nextIndex
        nextIndex = getNextIndex newECls 1
getNewDTransitions _ _ [] _ _ = []

-- | Creates origStates for single dtoState in single 'DTransition' with symbol specified in the parameters.
getNewDTransitions' :: [Transition]     -- ^ All transitions of FA 'FAutomata'
                -> ASymbol              -- ^ Single symbol of DFA 'DFAutomata'
                -> EpsClosure           -- ^ fromState in 'DTransition'
                -> [AState]             -- ^ origStates for dtoState in 'DTransition'
getNewDTransitions' (x:xs) a eps =
    if isElem then (toState x) : states
    else states
    where
        isElem = (fromState x) `elem` origStates eps && a == sym x
        states = getNewDTransitions' xs a eps
getNewDTransitions' [] _ _ = []

{-|
Parses highest index from array of states 'EpsClosure' represented as 'DState'.
Returns next free index.
-}
getNextIndex  :: [EpsClosure]     -- ^ States of DFA 'DFAutomata'
              -> Int              -- ^ Starting index for specifying minimum index
              -> Int              -- ^ Next free index
getNextIndex (x:xs) num = getNextIndex xs $ maximum [(stateIndex x),num]
getNextIndex [] num = num + 1

-- | Parses new states of DFA 'DFAutomata' from array of 'DTransition'.
getNewEClsFromDTrans :: [EpsClosure]    -- ^ All states of DFA
                    -> [DTransition]    -- ^ Transitions of DFA
                    -> [EpsClosure]     -- ^ New states of DFA
getNewEClsFromDTrans e (x:xs) =
    (getNewEClsFromDTrans' e x False) ++ (getNewEClsFromDTrans e xs)
getNewEClsFromDTrans _ [] = []

-- | Parses new states of DFA 'DFAutomata' from single 'DTransition'.
getNewEClsFromDTrans' :: [EpsClosure]   -- ^ All states of DFA
                    -> DTransition      -- ^ Single transition
                    -> Bool             -- ^ Flag if new state was found
                    -> [EpsClosure]     -- ^ If found returns empty array, otherwise new 'EpsClosure'
getNewEClsFromDTrans' (x:xs) t False =
    getNewEClsFromDTrans' xs t eq
    where
        eq = isEqEpsClosure x $ origStates $ dtoState t
getNewEClsFromDTrans' _ _ True = []
getNewEClsFromDTrans' [] t False = [(dtoState t )]

{-|
Checks if a state of DFA 'DFAutomata' with
same origStates exists. If that state doesn't exist, creates a new one.
Otherwise returns existing state. Works similar to Singleton pattern.
-}
createEpsClosure :: [EpsClosure]   -- ^ All states of DFA
                -> Int            -- ^ Potential index of the new state
                -> [AState]       -- ^ States in original FA 'FAutomata'
                -> EpsClosure     -- ^ State of the DFA with needed origStates
createEpsClosure [] num ss = (ECls num ss)
createEpsClosure (x:xs) num ss =
  if isEqEpsClosure x ss
  then x
  else createEpsClosure xs num ss

-- | Decides whether 'AState' array is equal to origStates of 'EpsClosure'.
isEqEpsClosure  :: EpsClosure     -- ^ Single state of DFA 'DFAutomata'
                -> [AState]       -- ^ Array of 'AState'
                -> Bool           -- ^ True if equal, False otherwise
isEqEpsClosure eps as = (sort $ origStates eps) == sort as

{-|
Creates origStates for 'EpsClosure' from array of 'Transition'.
It is practically epsilon closure. We have to check if exists
any transition in format x,,y where x belongs to states already
in origStates and y is a new state, that will be added to origStates.
Algorithm stops when we didn't add new state to origStates.
-}
getOrigStates :: [Transition]   -- ^ Transitions of FA 'FAutomata'
              -> [AState]       -- ^ States already in origStates
              -> [AState]       -- ^ origStates for 'EpsClosure'
getOrigStates t xs =
    if xs == nxs
    then xs
    else getOrigStates t nxs
    where
        nxs = getOrigStates' t xs

{-|
Iterates every 'Transition'. If epsilon trasition is found, checks if fromState
is in origStates. If it is and toState is not in origStates, then toState is added
to origStates.
-}
getOrigStates'  :: [Transition]     -- ^ Transitions of FA 'FAutomata'
                -> [AState]         -- ^ States already in origStates
                -> [AState]         -- ^ origStates for 'EpsClosure'
getOrigStates' (x:xs) ys =
    if null (sym x) && fromState x `elem` ys && (not $ toState x `elem` ys)
    then getOrigStates' xs $ (toState x) : ys
    else getOrigStates' xs ys
getOrigStates' [] ys = sort $ nub ys

-- | Identifies finite states in DFA 'DFAutomata'.
getFiniteStates :: [EpsClosure]   -- ^ States of DFA 'DFAutomata'
                -> [AState]       -- ^ Finite states of FA 'FAutomata'
                -> [EpsClosure]   -- ^ Finite states of DFA 'DFAutomata'
getFiniteStates [] _ = []
getFiniteStates xs ss = filter (isFinalState') xs
    where
        isFinalState' = isFinalState ss

-- | Decides whether a state of DFA 'DFAutomata' is final or not.
isFinalState :: [AState]       -- ^ Final states of FA 'FAutomata'
              -> EpsClosure     -- ^ State of DFA 'DFAutomata'
              -> Bool           -- ^ True if EpsClosure is finite state, False otherwise
isFinalState (x:xs) eps  =
    if x `elem` origStates eps then True else isFinalState xs eps
isFinalState [] _ = False

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120
