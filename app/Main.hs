module Main where

import Parser
import Interp
import Text.Show.Pretty(pPrint)
import System.Environment

main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ "app/" ++ head args
    pPrint $ parse input
    putStrLn $ run input
    return ()