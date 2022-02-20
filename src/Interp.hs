module Interp where

import Parser
import AST
import Util

import qualified Data.Map.Strict as M
import Data.Ratio
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S

data Context = Context {
                        names :: [M.Map Pattern Expression],
                        stdout :: String
                       }

lookupPattern :: [M.Map Pattern Expression] -> Pattern -> Expression
lookupPattern [] p = error $ "undefined name " ++ show p
lookupPattern (s:ss) p = orElse (M.lookup p s) (lookupPattern ss p)


tostring :: Expression -> String
tostring Null = "∅"
tostring (IntNum n) = show n
tostring (BoolVal b) = map toLower $ show b
tostring (Set s) = concat ["{",
                           intercalate ", " $ map tostring $ S.toList s,
                           "}"]
tostring (Ref _) = error "whoops"
tostring e = show e

interpret :: Context -> Statement -> Context
interpret c Nop = c
interpret c (Block stmts) = foldl interpret c' stmts
    where c' = Context {
                names = M.empty : names c,
                stdout = stdout c
               }
interpret c (Assign p e) =
    Context {
        names = mapHead (M.insert p e) (names c), -- TODO what if name already in scope?
        stdout = stdout c
    }
interpret c (Definition p e) = if M.member p (head $ names c)
                               then error "double definition"
                               else interpret c (Assign p e)
interpret c (Print e) =
    Context {
        names = names c,
        stdout = stdout c ++ tostring (evaluate c e) ++ "\n"  -- TODO what if evaluating changes the context?
    }
interpret _ _ = Context { names = [], stdout = "" }

evaluate :: Context -> Expression -> Expression
evaluate c Null = Null
evaluate c (IntNum n) = IntNum n
evaluate c (FracNum n) = if denominator n == 1 then IntNum $ numerator n
                                               else FracNum n
evaluate c (Prefix op e) = case (op, evaluate c e) of
    (Minus, IntNum n) -> IntNum (-n)
    (Plus, IntNum n) -> IntNum n

    (Empty, Null) -> BoolVal True
    (Empty, Set set) -> BoolVal $ null set
    (Even, IntNum n) -> BoolVal . even $ n
    (Odd, IntNum n) -> BoolVal . odd $ n
    (Neg, IntNum n) -> BoolVal (n < 0)
    (Neg, BoolVal b) -> BoolVal $ not b
    (_, _) -> error "type error" -- TODO better type errors
evaluate c (Infix op a b) = case (op, evaluate c a, evaluate c b) of
    (Add, IntNum a, IntNum b) -> IntNum (a + b)
    (Sub, IntNum a, IntNum b) -> IntNum (a - b)
    (Mul, IntNum a, IntNum b) -> IntNum (a * b)
    (Div, IntNum a, IntNum b) -> FracNum (a % b)

    (Equals, IntNum a, IntNum b) -> BoolVal $ a == b
    (Equals, FracNum a, FracNum b) -> BoolVal $ a == b
    (Equals, IntNum a, FracNum b) -> BoolVal False
    (Equals, FracNum a, IntNum b) -> BoolVal False
    (Equals, BoolVal a, BoolVal b) -> BoolVal $ a == b

    (NEquals, a, b) -> evaluate c (Prefix Neg $ Infix Equals a b)
    (Less, IntNum a, IntNum b) -> BoolVal $ a < b
    (Less, FracNum a, FracNum b) -> BoolVal $ a < b
    (Less, IntNum a, FracNum b) -> BoolVal $ (a % 1) < b
    (Less, FracNum a, IntNum b) -> BoolVal $ a < (b % 1)

    (_, _, _) -> error "type error" -- TODO better type errors
evaluate c (Set s) = Set $ S.map (evaluate c) s
evaluate c (Ref r) = lookupPattern (names c) r
evaluate c e = e

interpretFull :: Statement -> String
interpretFull = stdout . interpret Context { names = [], stdout = "" }

run :: String -> String
run = interpretFull . parse