module Main where

import Parser
import Interp
import Text.Show.Pretty(pPrint)

main :: IO ()
main = do
    input <- readFile "app/input.txt"
    pPrint $ parse input
    putStrLn $ run input
    return ()