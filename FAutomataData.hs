module FAutomataData where

import Data.List

type AState = String
type ASymbol = String

data Transition = Trans
    { fromState :: AState
    , fromSym :: ASymbol
    , toState :: AState
    } deriving (Eq)

instance Show Transition where
    show (Trans aq as af) =  "\n" ++ id aq ++ "," ++ id as ++ "," ++  id af

printTransition :: [Transition] -> String
printTransition (x:xs) = show x ++ printTransition xs
printTransition [] = []


printDTransition :: [DTransition] -> String
printDTransition (x:xs) = show x ++ printDTransition xs
printDTransition [] = []

data DTransition = DTrans
    { dfromState :: EpsClosure
    , dfromSym :: ASymbol
    , dtoState :: EpsClosure
    } deriving (Eq)

instance Show DTransition where
    show (DTrans aq as af) =  "\n" ++ show aq ++ "," ++ show as ++ "," ++  show af

data DFAutomata = DFA
    { dstates :: [EpsClosure]
    , dalphabet :: [ASymbol]
    , dtrans :: [DTransition]
    , dstart :: EpsClosure
    , dend :: [EpsClosure]
    } deriving (Eq)

instance Show DFAutomata where
    show (DFA q a t s f) = show q ++ "\n" 
    --show (DFA q a t s f) = show q ++ "\n" ++ show s ++ "\n" ++ show f ++ printDTransition t

data EpsClosure = ECls
    { stateName :: AState
    , origStates :: [AState]
    } deriving (Eq)

instance Show EpsClosure where
    show (ECls sn os) = "\n" ++ show sn ++ "\n" ++ show os ++ "\n"

data FAutomata = FA
    { states :: [AState]
    , alphabet :: [ASymbol]
    , trans :: [Transition]
    , start :: AState
    , end :: [AState]
    } deriving (Eq)

instance Show FAutomata where
    show (FA q a t s f) = id intercalate "," q ++ "\n" ++ id s ++ "\n" ++ id intercalate "," f ++ printTransition t

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120
