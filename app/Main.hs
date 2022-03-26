module Main where

import Control.Monad.State.Lazy

import Parser
import Interp
import Mather
import Text.Show.Pretty(pPrint)
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs 
    input <- readFile $ "app/" ++ head args
    pPrint $ parse input
    pPrint $ execState <$> purifyExpr (runParser expressionParser input) <*> Just emptyPurif
    run input
    return ()