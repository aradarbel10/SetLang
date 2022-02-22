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

type Scope = M.Map Pattern Expression
type Scopes = [Scope];
data Context = Context {
                        names :: Scopes,
                        stdout :: String
                       }

patternName :: Pattern -> String
patternName (Pattern n _) = n

assignInScopeList :: [(Pattern, Expression)] -> String -> Expression -> Maybe [(Pattern, Expression)]
assignInScopeList [] _ _ = Nothing
assignInScopeList ((patt, curr):rest) name exp
    | patternName patt == name = Just $ (patt, exp):rest
    | otherwise = case assignInScopeList rest name exp of
        Nothing -> Nothing
        Just res -> Just $ (patt, curr):res

assignInScopes :: Scopes -> String -> Expression -> Maybe Scopes
assignInScopes [] name _ = Nothing
assignInScopes (s:ss) name e =
    case assignInScopeList (M.toAscList s) name e of
        Nothing -> (s:) <$> assignInScopes ss name e
        Just pairs -> Just $ M.fromAscList pairs : ss
        
lookupInScopeList :: [(Pattern, Expression)] -> String -> Maybe (Pattern, Expression)
lookupInScopeList [] _ = Nothing
lookupInScopeList ((patt, exp):rest) name
    | patternName patt == name = Just (patt, exp)
    | otherwise = lookupInScopeList rest name

lookupInScopes :: Scopes -> String -> Maybe (Pattern, Expression)
lookupInScopes [] _ = Nothing
lookupInScopes (s:ss) name =
    case lookupInScopeList (M.toAscList s) name of
        Nothing -> lookupInScopes ss name
        Just exp -> Just exp

scopeParams :: [Expression] -> [(String, Type)] -> Maybe Scope
scopeParams [] [] = Just M.empty
scopeParams [] _  = Nothing
scopeParams _  [] = Nothing
scopeParams (exp:exps) ((name, typ):params) = M.insert (Pattern name []) exp <$> scopeParams exps params

applyOn :: Scope -> Expression -> Expression
applyOn scope = evaluate (Context { names = [scope], stdout = "" })

tostring :: Expression -> String
tostring Null = "∅"
tostring (IntNum n) = show n
tostring (BoolVal b) = map toLower $ show b
tostring (Set s) = concat ["{",
                           intercalate ", " $ map tostring $ S.toList s,
                           "}"]
tostring (Ref name) = "varname " ++ name
tostring e = show e

interpret :: Context -> Statement -> Context
interpret c Nop = c
interpret c (Block stmts) =
            let c' = Context {
                names = M.empty : names c,
                stdout = stdout c
            }
            in foldl interpret c' stmts
interpret c (Definition p e)
    | null (names c) = error "no scope"
    | M.member p (head $ names c) = error "double definition"
    | otherwise = Context {
                      names = mapHead (M.insert p e) (names c),
                      stdout = stdout c
                  }
interpret c (Assign p e) =
    case assignInScopes (names c) (patternName p) e of
        Nothing -> Context { names = mapHead (M.insert p e) (names c),
                             stdout = stdout c }
        Just names' -> Context { names = names', stdout = stdout c }
interpret c (Print e) =
    Context {
        names = names c,
        stdout = stdout c ++ tostring (evaluate c e) ++ "\n"  -- TODO what if evaluating changes the context?
    }
interpret c (Exec exp) = execute c (evaluate c exp)
interpret _ _ = error "not implemented"

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
evaluate c (Applic (Ref name) ps) =
    case lookupInScopes (names c) name of
        Nothing -> error $ "undefined " ++ name
        Just (Pattern name' params, body) -> case scopeParams (map (evaluate c) ps) params of
            Nothing -> error "bad application"
            Just scope -> applyOn scope body
evaluate c (Applic _ _) = error "cant apply on non callable"
evaluate c (Ref name) = case lookupInScopes (names c) name of
    Nothing -> error $ "undefined name " ++ show name
    Just (_, expr) -> evaluate c expr
evaluate c e = e

execute :: Context -> Expression -> Context
execute c (Proc stmt) = interpret c stmt
execute c (Applic (Ref name) ps) =
    case lookupInScopes (names c) name of
        Nothing -> error $ "undefined " ++ name
        Just (Pattern name' params, body) -> case scopeParams (map (evaluate c) ps) params of
            Nothing -> error "bad application"
            Just scope -> error $ "debug: " ++ show body--execute (Context { names = scope:names c, stdout = stdout c }) body
execute c (Applic _ _) = error "cant apply on non callable"
execute c _ = c

interpretFull :: Statement -> String
interpretFull = stdout . interpret Context { names = [], stdout = "" }

run :: String -> String
run = interpretFull . parse