module AutomataData where


type AInput = String
type AState = Char
type ASymbol = Char

data Transition = Trans
	{ fromState :: AState
	, sym :: ASymbol
	, toState :: AState
	} deriving (Eq)

instance Show Transition where
	show (Trans fq fs tq ta) = "\n\t" ++ show fq ++ "," ++ show fs ++ "," ++ show tq ++ "," ++ show ta

-- M = (Q, Σ, δ, q0, F)
data AMachine = AM
	{ states :: [AState]
	, alphabet :: [ASymbol]
	, trans :: [Transition]
	, start :: [AState]
	, end :: AState
	} deriving (Eq)

instance Show AMachine where
	show (AM q a t s f) = show q ++ "\n" ++ show a ++ "\n" ++ show t ++ "\n" ++ show s ++ "\n" ++ show f

--
-- data Tape a = Tape a [a] [a]

--instance (Show a) => Show (Tape a) where
--	show (Tape x lts rts) = show (reverse $ take 20 lts) ++ "[" ++ (show x) ++ "]" ++ show (take 20 rts)
