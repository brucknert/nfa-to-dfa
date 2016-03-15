{- |
Module      :  FAutomataMain
Description :  Starting point of the program.
Copyright   :  (c) Tomas Bruckner 2016
License     :  MIT

Maintainer  :  xbruck02@stud.fit.vutbr.cz
Stability   :  stable
Portability :  portable

[@Example program run::@]

  -./FAutomataMain options [input_file]

[@options:@]

Parameters are mutually exclusive and either one  of them must be specified.

  -\-i   - prints input Finite Automata 'FAutomata'

  -\-t  - transforms Finite Automata 'FAutomata' with epsilon transitions to Deterministic Finite Automata 'DFAutomata'

[@input_file:@]

  -stdin is used if name of the file is not provided
-}

module Main(main) where

import System.IO
import System.Environment
import FAutomataData
import FAutomataFuncs

-- |Starting point of the program.
main :: IO ()   -- ^ Program output
main = do
    args <- getArgs
    let (transform, inFile) = procArgs args
    content <- if inFile == "stdin"
        then hGetContents stdin
        else do
            hInFile <- openFile inFile ReadMode
            content <- hGetContents hInFile
            return content
    fa <- getFiniteAutomata content
    if transform
        then transformFiniteAutomata fa
        else dumpFiniteAutomata fa
    return ()

{-|
Processes program arguments.
Returns program input file or stdin and information if program should dump or transform Finite Automata.
-}
procArgs  :: [String]       -- ^ Progam arguments
          -> (Bool,String)  -- ^ Bool represents -i/-t option, String input_file
procArgs [x]
    | x=="-i" = (False, "stdin")
    | x=="-t" = (True, "stdin")
    | otherwise = error "unknown argument"
procArgs [x,y]
    | x=="-i" = (False, y)
    | x=="-t" = (True, y)
    | otherwise = error "unknown arguments"
procArgs _ = error "unknown arguments"

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120
