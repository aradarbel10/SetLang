{-# LANGUAGE TupleSections #-}
module Interp where

import Parser
import AST
import Util

import qualified Data.Map.Strict as M
import Data.Ratio ( (%), denominator, numerator )
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Control.Monad.State.Lazy
import Data.Functor

import Debug.Trace
debug = flip trace

reContext :: (Scopes -> Scopes) -> (String -> String) -> State Context ()
reContext fnames fstdout = state $ \c -> () ! Context {
    names = fnames $ names c,
    stdout = fstdout $ stdout c
}

getNames :: State Context Scopes
getNames = state $ \c -> (names c, c)

getStdout :: State Context String
getStdout = state $ \c -> (stdout c, c)

putNames :: Scopes -> State Context ()
putNames ns = state $ \c -> () ! Context { names = ns, stdout = stdout c }

appendScope :: Scope -> State Context ()
appendScope scope = reContext (M.empty:) id




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

data Application = Bad
                 | Full Scope
                 | Partial Integer

tryApplyParams :: [Expression] -> [(String, Type)] -> Application
tryApplyParams [] [] = Full M.empty
tryApplyParams [] left = Partial $ toInteger $ length left
tryApplyParams _ [] = Bad
tryApplyParams (exp:exps) ((name, typ):params) =
    let scope = tryApplyParams exps params in
    case scope of
        Bad -> Bad
        Full scope -> Full $ M.insert (Pattern name []) exp scope
        Partial i -> Partial i

applyOn :: Scopes -> Expression -> State Context Expression
applyOn names e = do
    names' <- getNames
    putNames names
    res <- evaluate e
    putNames names'
    return res
    
tostring :: Expression -> String
tostring Null = "∅"
tostring (IntNum n) = show n
tostring (BoolVal b) = map toLower $ show b
tostring (Set s) = concat ["{",
                           intercalate ", " $ map tostring $ S.toList s,
                           "}"]
tostring (Ref name) = "varname " ++ name
tostring e = show e

interpret :: Statement -> State Context ()
interpret Nop =
    return ()
interpret (Block []) =
    return ()
interpret (Block (stmt:rest)) = do
    appendScope M.empty
    interpret stmt
    interpret $ Block rest
interpret (Definition p e) = do
    ns <- getNames
    if null ns
        then error "no scope"
        else if M.member p (head ns) -- TODO use proper lookup to check for duplicates
            then error "double definition"
            else reContext (mapHead (M.insert p e)) id
interpret (Assign p e) = do
    ns <- getNames
    res <- evaluate e
    case assignInScopes ns (patternName p) Null of
        Nothing -> reContext (mapHead (M.insert p res)) id
        Just ns' -> putNames ns'
interpret (Print e) = do
    val <- evaluate e
    reContext id (++ "\n" ++ tostring val)
interpret (Exec e) = do
    evaluate e
    return ()
interpret _ = error "not implemented"


evaluate :: Expression -> State Context Expression
evaluate Null = return Null
evaluate (IntNum n) = return $ IntNum n
evaluate (FracNum n) = return $
    if denominator n == 1
        then IntNum $ numerator n
        else FracNum n
evaluate (Prefix op e) = do
    e' <- evaluate e
    return $ case (op, e') of
        (Minus, IntNum n) -> IntNum (-n)
        (Plus, IntNum n) -> IntNum n

        (Empty, Null) -> BoolVal True
        (Empty, Set set) -> BoolVal $ null set
        (Even, IntNum n) -> BoolVal . even $ n
        (Odd, IntNum n) -> BoolVal . odd $ n
        (Neg, IntNum n) -> BoolVal (n < 0)
        (Neg, BoolVal b) -> BoolVal $ not b
        (_, _) -> error "type error" -- TODO better type errors
evaluate (Infix op a b) = do
    lhs <- evaluate a
    rhs <- evaluate b
    if op == NEquals
        then evaluate (Prefix Neg $ Infix Equals lhs rhs)
        else return $ case (op, lhs, rhs) of
            (Add, IntNum a, IntNum b) -> IntNum (a + b)
            (Sub, IntNum a, IntNum b) -> IntNum (a - b)
            (Mul, IntNum a, IntNum b) -> IntNum (a * b)
            (Div, IntNum a, IntNum b) -> FracNum (a % b)

            (Equals, IntNum a, IntNum b) -> BoolVal $ a == b
            (Equals, FracNum a, FracNum b) -> BoolVal $ a == b
            (Equals, IntNum a, FracNum b) -> BoolVal False
            (Equals, FracNum a, IntNum b) -> BoolVal False
            (Equals, BoolVal a, BoolVal b) -> BoolVal $ a == b

            (Less, IntNum a, IntNum b) -> BoolVal $ a < b
            (Less, FracNum a, FracNum b) -> BoolVal $ a < b
            (Less, IntNum a, FracNum b) -> BoolVal $ (a % 1) < b
            (Less, FracNum a, IntNum b) -> BoolVal $ a < (b % 1)

            (_, _, _) -> error "type error" -- TODO better type errors
evaluate (Set s) =
    Set <$> foldl
        (\acc expr -> do
            set <- acc
            res <- evaluate expr
            return $ S.insert res set
        )
        (return S.empty)
        s

evaluate (Applic (Applic func curried) []) =
    evaluate $ Applic func curried
evaluate (Applic (Applic func curried) ps) =
    evaluate $ Applic func (curried ++ ps)
evaluate (Applic (Func patt exp) params) =
    let (Pattern name sig) = patt
        es = foldl
            (\acc expr -> do
                ps <- acc
                res <- evaluate expr
                return $ res:ps
            )
            (return [])
            params
    in do
        app <- es <&> \ps -> tryApplyParams ps sig
        case app of
            Bad -> error "bad application"
            Partial i -> return $ Applic (Applic (Func patt exp) params) []
            Full scope -> do
                ns <- getNames
                applyOn (scope:ns) exp
evaluate (Applic Null _) = error "cant apply on Null"
evaluate (Applic func ps) = do
    func' <- evaluate func
    evaluate $ Applic func' ps

evaluate (Proc stmt) = do
    res <- interpret stmt
    return Null

evaluate (Ref name) = do
    ns <- getNames
    case lookupInScopes ns name of
        Nothing -> error $ "undefined name " ++ show name
        Just (patt, expr) ->
            case patt of
                Pattern _ [] -> return expr
                Pattern _ ps -> return $ Func patt expr
evaluate e = return e

interpretFull :: Statement -> String
interpretFull stmt = stdout $ execState (interpret stmt) Context { names = [], stdout = "" }

run :: String -> String
run = interpretFull . parse