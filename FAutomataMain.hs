{-|
Module      : FAutomataMain
Description : Main
Course      : FLP - Functional and Logic Programming
Author      : Tomas Bruckner, xbruck02@stud.fit.vutbr.cz
Date        : 2016-03-14
-}
module Main(main) where

import System.IO
import System.Environment
import FAutomataData
import FAutomataFuncs

main :: IO ()
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

procArgs :: [String] -> (Bool,String)
procArgs [x]
    | x=="-i" = (False, "stdin")
    | x=="-t" = (True, "stdin")
    | otherwise = error "unknown arguments"
procArgs [x,y]
    | x=="-i" = (False, y)
    | x=="-t" = (True, y)
    | otherwise = error "unknown arguments"
procArgs _ = error "unknown arguments"

-- vim: expandtab:shiftwidth=4:tabstop=4:softtabstop=0:textwidth=120
