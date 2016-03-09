-- export everything in this module
module TuringData where

-- export only selected objects
-- module TuringData ( TSMachine ) where

-- synonyms helps to understand semantics
type TInput = String
type TState = String
type TSymbol = Char

--
data Action
	= ALeft
	| ARight
	| AWrite TSymbol
   deriving (Eq, Show)

--
	, fromSym :: TSymbol
	, toState :: TState
	, toAction :: Action
	} deriving (Eq)

instance Show Transition where
	show (Trans fq fs tq ta) = "\n\t" ++ show fq ++ "," ++ show fs ++ "," ++ show tq ++ "," ++ show ta

--
data TSMachine = TSM
	{ states :: [TState]
	, alphabet :: [TSymbol]
	, trans :: [Transition]
	, start :: TState
	, end :: TState
	} deriving (Eq)

instance Show TSMachine where
	show (TSM q a t s f) = show q ++ "\n" ++ show a ++ "\n" ++ show t ++ "\n" ++ show s ++ "\n" ++ show f

--
data Tape a = Tape a [a] [a]

instance (Show a) => Show (Tape a) where
	show (Tape x lts rts) = show (reverse $ take 20 lts) ++ "[" ++ (show x) ++ "]" ++ show (take 20 rts)
