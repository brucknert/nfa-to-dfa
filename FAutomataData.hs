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
    show (Trans aq as af) =  id aq ++ "," ++ id as ++ "," ++  id af ++ "\n"

printTransition :: [Transition] -> String
printTransition (x:xs) = show x ++ printTransition xs
printTransition [] = [] 

data FAutomata = FA
    { states :: [AState]
    , alphabet :: [ASymbol]
    , trans :: [Transition]
    , start :: AState
    , end :: [AState]
    } deriving (Eq)

instance Show FAutomata where
    show (FA q a t s f) = id intercalate "," q ++ "\n" ++ id s ++ "\n" ++ id intercalate "," f ++ "\n" ++ printTransition t 

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120

