module FAutomataData where

type AState = String
type ASymbol = Char

data Transition = Trans
	{ fromState :: AState
	, fromSym :: ASymbol
	, toState :: AState
	} deriving (Eq)

instance Show Transition where
	show (Trans aq as af) = "\n\t" ++ show aq ++ "," ++ show as ++ "," ++  show af

data FAutomata = FA
	{ states :: [AState]
	, alphabet :: [ASymbol]
	, trans :: [Transition]
	, start :: AState
	, end :: [AState]
	} deriving (Eq)

instance Show FAutomata where
	show (FA q a t s f) = show q ++ "\n" ++ show a ++ "\n" ++ show t ++ "\n" ++ show s ++ "\n" ++ show f

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120

