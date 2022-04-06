module Main where

import Control.Monad.State.Lazy

import Parser
import Interp
import Mather
import Text.Show.Pretty(pPrint)
import System.Environment
import System.IO

main :: IO ()
{-
main = do
    args <- getArgs 
    input <- readFile $ "app/" ++ head args
    pPrint $ parse input
    pPrint $ execState <$> purifyExpr (runParser expressionParser input) <*> Just emptyPurif
    run input
    return ()
-}

main = do
    let input = "x + 2 = y and (f (car (cons x a))) = (f (y - x + 1))"
    let expr = runParser expressionParser input
    pPrint expr
    let pured = execState (purifyPred expr) emptyPurif
    pPrint pured
    return ()