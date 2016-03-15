{- |
Module      :  FAutomataData
Description :  Data structures and types.
Copyright   :  (c) Tomas Bruckner 2016
License     :  MIT

Maintainer  :  xbruck02@stud.fit.vutbr.cz
Stability   :  stable
Portability :  portable

Data structures and types.
-}

module FAutomataData where

import Data.List

-- | Represents state in Finite Automata 'FAutomata'
type AState = String

-- | Represents symbol of alphabet in both 'FAutomata' and 'DFAutomata'
type ASymbol = String

-- | Represents state in Deterministic Finite Automata 'DFAutomata'
type DState = Int

-- | Describes Finite Automata with epsilon transitions that is used as input for this program.
data FAutomata = FA
      { states :: [AState]      -- ^ All states
      , alphabet :: [ASymbol]   -- ^ Alphabet
      , trans :: [Transition]   -- ^ Transitions
      , initial :: AState       -- ^ Initial state
      , finals :: [AState]      -- ^ Final states
      } deriving (Eq)

-- | Prints Finite Automata 'FAutomata' in desirable format.
instance Show FAutomata where
    show (FA q _ t s f) =
        id intercalate "," q ++ "\n" ++ id s ++ "\n" ++ id intercalate "," f ++ printTransition t

{-|
Describes single transition in Finite Automata 'FAutomata'.
It is not used for Deterministic Finite Automata 'DFAutomata'.
-}
data Transition = Trans
    { fromState :: AState   -- ^ Actual state
    , sym :: ASymbol    -- ^ Input symbol, empty string for epsilon
    , toState :: AState     -- ^ Next state
    } deriving (Eq)

-- | Prints Transition 'Transition' in desirable format.
instance Show Transition where
    show (Trans aq as af) =  "\n" ++ id aq ++ "," ++ id as ++ "," ++  id af

-- | Prints array of 'Transition' in desirable format.
printTransition :: [Transition]   -- ^ Array of 'Transition' for printing
                -> String         -- ^ Output 'String'
printTransition (x:xs) = show x ++ printTransition xs
printTransition [] = []

-- | Describes Deterministic Finite Automata that is used as output for this program.
data DFAutomata = DFA
    { dstates :: [EpsClosure]   -- ^ All states
    , dalphabet :: [ASymbol]    -- ^ Alphabet
    , dtrans :: [DTransition]   -- ^ Transitions
    , dinitial :: EpsClosure      -- ^ Initial state
    , dfinals :: [EpsClosure]      -- ^ Final states
    } deriving (Eq)

-- | Prints DFAutomata 'DFAutomata' in desirable format.
instance Show DFAutomata where
    show (DFA q _ t s f) = --show q
        printECls q ++ "\n" ++ printECls [s] ++ "\n" ++ printECls f ++ printDTransition t

-- | Single state of Deterministic Finite Automata 'DFAutomata'.
data EpsClosure = ECls
    { stateIndex :: DState       -- ^ Name of the state
    , origStates :: [AState]    -- ^ Represents states in original Finite Automata 'FAutomata'
    } deriving (Eq)

-- | Prints EpsClosure 'EpsClosure' in desirable format.
instance Show EpsClosure where
    show (ECls s ss) = show s ++ show ss

-- | Prints array of states of Deterministic Finite Automata 'DFAutomata'.
printECls :: [EpsClosure]   -- ^ Array of 'EpsClosure' for printing
          -> String         -- ^ Output 'String'
printECls [(ECls x _)] = show x
printECls ((ECls x _):xs) = show x ++ "," ++ printECls xs
printECls [] = []

{-|
Describes single transition in Deterministic Finite Automata 'DFAutomata'.
It is not used for Finite Automata 'FAutomata'.
-}
data DTransition = DTrans
    { dfromState :: EpsClosure  -- ^ Actual state
    , dsym :: ASymbol       -- ^ Input symbol, cannot be epsilon
    , dtoState :: EpsClosure    -- ^ Next state
    } deriving (Eq)

-- | Prints DTransition 'DTransition' in desirable format.
instance Show DTransition where
    show (DTrans ds s ts) = show ds ++ show s ++ show ts

-- | Prints array 'DTransition'.
printDTransition  :: [DTransition]  -- ^ Array of 'DTransition' for printing
                  -> String         -- ^ Output 'String' in desirable format
printDTransition [(DTrans fs s ts)] =
    "\n" ++ printECls [fs] ++ "," ++ id s ++ "," ++ printECls [ts]
printDTransition (x:xs) = printDTransition [x] ++ printDTransition xs
printDTransition [] = []

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120
