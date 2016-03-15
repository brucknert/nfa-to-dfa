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
transformFiniteAutomata :: FAutomata  -- ^ Finite Automata that should be trasnformed
                        -> IO ()      -- ^ Output Deterministic Finite Automata
transformFiniteAutomata fa@(FA q a t s f)  = do
    let q0 = getInitialState t s
    let dfa = DFA [q0] a [] q0 []
    let tdfa = getDFAutomata dfa fa [q0]
    putStrLn $ show tdfa
    return ()

{-|
Transforms empty Deterministic Finite Automata 'DFAutomata' from Finite Automata 'FAutomata'.
Returns transformed transformed Deterministic Finite Automata 'DFAutomata'.
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
      dtrans = getNewTransition ds (trans fa) (alphabet fa) x (getNextIndex ds 0)
      newstates = getNewEps ds dtrans
getDFAutomata dfa _ [] = dfa

{-|
Parses highest index from array of states 'EpsClosure' represented as 'DState'.
Returns next free index.
-}
getNextIndex  :: [EpsClosure]     -- ^ States of Deterministic Finite Automata 'DFAutomata'
              -> Int              -- ^ Starting index for specifying minimum index
              -> Int              -- ^ Next free index
getNextIndex (x:xs) num = getNextIndex xs $ maximum [(stateIndex x),num]
getNextIndex [] num = num + 1

-- | Identifies finite states in Deterministic Finite Automata 'DFAutomata'.
getFiniteStates :: [EpsClosure]   -- ^ States of Deterministic Finite Automata 'DFAutomata'
                -> [AState]       -- ^ Finite states of Finite Automata 'FAutomata'
                -> [EpsClosure]   -- ^ Finite states of Deterministic Finite Automata 'DFAutomata'
getFiniteStates [] _ = []
getFiniteStates xs ss = filter (isFinalState') xs
    where
        isFinalState' = isFinalState ss

-- |
getNewEps :: [EpsClosure] -> [DTransition] -> [EpsClosure]
getNewEps e (x:xs) = (getNewEps' e x False) ++ (getNewEps e xs)
getNewEps _ [] = []

-- |
getNewEps' :: [EpsClosure] -> DTransition -> Bool -> [EpsClosure]
getNewEps' (x:xs) t False = getNewEps' xs t eq
    where
        eq = isEqEpsClosure x $ origStates $ dtoState t
getNewEps' _ _ True = []
getNewEps' [] t False = [(dtoState t )]

-- | Parses Finite Automata's alphabet from array of 'Transition' and removes duplicity.
getAlphabet :: [Transition]   -- ^ Transitions of Finite Automata 'FAutomata'
            -> [ASymbol]      -- ^ Finite Automata's alphabet
getAlphabet (x:xs) = nub $ sym x : getAlphabet xs
getAlphabet [] = []

-- | Decides whether a state of Deterministic Finite Automata 'DFAutomata' is final or not.
isFinalState :: [AState]       -- ^ Final states of Finite Automata 'FAutomata'
              -> EpsClosure     -- ^ State of Deterministic Finite Automata 'DFAutomata'
              -> Bool           -- ^ True if EpsClosure is finite state, False otherwise
isFinalState (x:xs) eps  =
    if x `elem` origStates eps then True else isFinalState xs eps
isFinalState [] _ = False

-- |
getNewTransition    :: [EpsClosure]
                    -> [Transition]
                    -> [ASymbol]
                    -> EpsClosure
                    -> Int
                    -> [DTransition]
getNewTransition ds t (a:as) eps num =
    if null nts
    then (getNewTransition ds t (as) eps num)
    else (DTrans eps a newState) : (getNewTransition newECls t (as) eps (getNextIndex newECls 1))
    where
        nts = getNewTransition' t a eps
        newState = createEpsClosure ds num $ sort (getEpsClosure t nts)
        newECls = nub $ newState : ds
getNewTransition _ _ [] _ _ = []

-- |
getNewTransition'   :: [Transition]
                    -> ASymbol
                    -> EpsClosure
                    -> [AState]
getNewTransition' (x:xs) a eps =
    if isElem then (toState x) : states
    else states
    where
        isElem = (fromState x) `elem` origStates eps && a == sym x
        states = getNewTransition' xs a eps
getNewTransition' [] _ _ = []

{-|
Checks if a state of Deterministic Finite Automata 'DFAutomata' with
same origStates exists. If that state doesn't exist, creates a new one.
Otherwise returns existing state. Works similar to Singleton pattern.
-}
createEpsClosure :: [EpsClosure]   -- ^ All states of DFA
                -> Int            -- ^ Potential index of the new state
                -> [AState]       -- ^ States in original Finite Automata 'FAutomata'
                -> EpsClosure     -- ^ State of the DFA with needed origStates
createEpsClosure [] num ss = (ECls num ss)
createEpsClosure (x:xs) num ss =
  if isEqEpsClosure x ss
  then x
  else createEpsClosure xs num ss

-- | Decides whether 'AState' array is equal to origStates of 'EpsClosure'.
isEqEpsClosure  :: EpsClosure     -- ^ Single state of Deterministic Finite Automata 'DFAutomata'
                -> [AState]       -- ^ Array of 'AState'
                -> Bool           -- ^ True if equal, False otherwise
isEqEpsClosure eps as = (sort $ origStates eps) == sort as

-- | Parses initial state for Deterministic Finite Automata 'DFAutomata' from array of 'Transition'.
getInitialState :: [Transition]   -- ^ Transitions of Finite Automata 'FAutomata'
                -> AState         -- ^ Initial state of Finite Automata 'FAutomata'
                -> EpsClosure     -- ^ Initial state of Deterministic Finite Automata 'DFAutomata'
getInitialState t s = ECls 1 $ sort $ getEpsClosure t [s]

-- |
getEpsClosure :: [Transition]
              -> [AState]
              -> [AState]
getEpsClosure t xs =
    if xs == nxs then nub xs
    else nub $ getEpsClosure' t nxs
    where
        nxs =  getEpsClosure' t xs

-- | Creates
getEpsClosure'  :: [Transition]
                -> [AState]
                -> [AState]
getEpsClosure' (x:xs) ys =
    if null (sym x) && fromState x `elem` ys
    then getEpsClosure' xs $ (toState x) : ys
    else getEpsClosure' xs ys
getEpsClosure' [] ys = sort ys

-- | Parses program input and returns Finite Automata 'FAutomata'.
procLns :: [String]     -- ^ Lines of program's input
        -> FAutomata    -- ^ Parsed Finite Automata 'FAutomata'
procLns (states:[initial]:final:transitions) =
    if null transitions then error "no transitions"
    else FA getStates getAlph rules [initial] getFinal
    where
        getStates = splitOn "," states
        getFinal = splitOn "," final
        rules = map getRule transitions
        getAlph = delete "" $ sort (getAlphabet rules)
        getRule rule = getRule' $ splitOn "," rule
        getRule' :: [String] -> Transition
        getRule' [q1,sym,q2] =  Trans q1 sym q2
        getRule' x = error "bad transition syntax"
procLns _ = error "bad syntax"

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120
