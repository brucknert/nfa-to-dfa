module FAutomataData where

import Data.List

type AState = String
type ASymbol = String
type DState = Int

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

data DTransition = DTrans
    { dfromState :: EpsClosure
    , dfromSym :: ASymbol
    , dtoState :: EpsClosure
    } deriving (Eq)

data DFAutomata = DFA
    { dstates :: [EpsClosure]
    , dalphabet :: [ASymbol]
    , dtrans :: [DTransition]
    , dstart :: EpsClosure
    , dend :: [EpsClosure]
    } deriving (Eq)

instance Show DFAutomata where
    show (DFA q _ t s f) =
      printECls q ++ "\n" ++ printECls [s] ++ "\n" ++ printECls f ++ printDTransition t

data EpsClosure = ECls
    { stateName :: DState
    , origStates :: [AState]
    } deriving (Eq)

printDTransition :: [DTransition] -> String
printDTransition [(DTrans ds s ts)] = "\n" ++ printECls [ds] ++ "," ++ id s ++ "," ++ printECls [ts]
printDTransition (x:xs) = printDTransition [x] ++ printDTransition xs
printDTransition [] = []

printECls :: [EpsClosure] -> String
printECls [(ECls x _)] = show x
printECls ((ECls x _):xs) = show x ++ "," ++ printECls xs
printECls [] = []

data FAutomata = FA
    { states :: [AState]
    , alphabet :: [ASymbol]
    , trans :: [Transition]
    , start :: AState
    , end :: [AState]
    } deriving (Eq)

instance Show FAutomata where
    show (FA q _ t s f) = id intercalate "," q ++ "\n" ++ id s ++ "\n" ++ id intercalate "," f ++ printTransition t

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120
