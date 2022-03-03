module Interp where

import Parser
import AST
import Util
import qualified Mather as TH

import qualified Data.Map.Strict as M
import Data.Ratio ( (%), denominator, numerator )
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Control.Monad.State.Lazy
import Control.Monad.Trans
import Data.Functor
import System.IO

import Debug.Trace
debug = flip trace

reContext :: (Scopes -> Scopes) -> StateT Context IO ()
reContext fnames = state $ \c -> () ! Context {
    names = fnames $ names c
}

getNames :: StateT Context IO Scopes
getNames = state $ \c -> (names c, c)

putNames :: Scopes -> StateT Context IO ()
putNames ns = state $ \c -> () ! Context { names = ns }

pushScope :: Scope -> StateT Context IO ()
pushScope scope = reContext (M.empty:)

popScope :: StateT Context IO ()
popScope = reContext tail


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

lookupByName :: M.Map Pattern a -> String -> Maybe (Pattern, a)
lookupByName map = helper (M.toList map)
    where helper []                 _ = Nothing
          helper ((patt, val):rest) n
              | patternName patt == n = Just (patt, val)
              | otherwise             = helper rest n

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

applyOn :: Scopes -> Expression -> StateT Context IO Expression
applyOn names e = do
    names' <- getNames
    putNames names
    res <- evaluate e
    putNames names'
    return res

tostring :: Expression -> String
tostring EmptySet = "∅"
tostring (IntNum n) = show n
tostring (BoolVal b) = map toLower $ show b
tostring (RastorSet s) = concat ["{",
                           intercalate ", " $ map tostring $ S.toList s,
                           "}"]
tostring (Ref name) = "varname " ++ name
tostring (StrVal str) = str
tostring e = show e

interpret :: Statement -> StateT Context IO ()
interpret Nop =
    return ()
interpret Echo = do
    line <- liftIO getLine
    liftIO $ putStrLn line
    return ()
interpret (Block stmts) = do
    pushScope M.empty
    foldl
        (\acc stmt -> acc >> interpret stmt)
        (return ())
        stmts
    popScope
interpret (Definition p e) = do
    ns <- getNames
    if null ns
        then error "no scope"
        else if M.member p (head ns) -- TODO use proper lookup to check for duplicates
            then error "double definition"
            else reContext (mapHead (M.insert p e))
interpret (Assign p e) = do
    ns <- getNames
    res <- evaluate e
    case assignInScopes ns (patternName p) EmptySet of
        Nothing -> reContext (mapHead (M.insert p res))
        Just ns' -> putNames ns'
interpret (Exec e) = do
    evaluate e
    return ()
interpret (IfStatement c tb fb) = do
    cond <- evaluate c

    if cond == BoolVal True then interpret tb
    else if cond == BoolVal False then forM_ fb interpret
    else error "if-then-else condition must be a boolean"
    return ()
interpret _ = error "not implemented"


evaluate :: Expression -> StateT Context IO Expression
evaluate EmptySet = return EmptySet
evaluate (IntNum n) = return $ IntNum n
evaluate (FracNum n) = return $
    if denominator n == 1
        then IntNum $ numerator n
        else FracNum n

evaluate (Ref "read") = do
    line <- liftIO getLine
    liftIO $ evalStateT (evaluate (Parser.runParser Parser.expressionParser line)) (Context { names = [] })
evaluate (Applic (Ref "write") [arg]) = do
    expr <- evaluate arg
    liftIO $ putStr (tostring expr) >> hFlush stdout
    return EmptySet
evaluate (Applic (Ref "writeln") [arg]) = do
    evaluate (Applic (Ref "write") [arg])
    liftIO $ putStrLn ""
    return EmptySet

evaluate (Prefix op e) = do
    e' <- evaluate e
    return $ case (op, e') of
        (Minus, IntNum n) -> IntNum (-n)
        (Plus, IntNum n) -> IntNum n

        (Empty, EmptySet) -> BoolVal True
        (Empty, RastorSet set) -> BoolVal $ null set
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

            (BAnd, BoolVal a, BoolVal b) -> BoolVal $ a && b
            (BOr, BoolVal a, BoolVal b) -> BoolVal $ a || b

            (Less, IntNum a, IntNum b) -> BoolVal $ a < b
            (Less, FracNum a, FracNum b) -> BoolVal $ a < b
            (Less, IntNum a, FracNum b) -> BoolVal $ (a % 1) < b
            (Less, FracNum a, IntNum b) -> BoolVal $ a < (b % 1)

            (Greater, IntNum a, IntNum b) -> BoolVal $ a > b
            (Greater, FracNum a, FracNum b) -> BoolVal $ a > b
            (Greater, IntNum a, FracNum b) -> BoolVal $ (a % 1) > b
            (Greater, FracNum a, IntNum b) -> BoolVal $ a > (b % 1)

            (LessEq, IntNum a, IntNum b) -> BoolVal $ a <= b
            (LessEq, FracNum a, FracNum b) -> BoolVal $ a <= b
            (LessEq, IntNum a, FracNum b) -> BoolVal $ (a % 1) <= b
            (LessEq, FracNum a, IntNum b) -> BoolVal $ a <= (b % 1)

            (GreaterEq, IntNum a, IntNum b) -> BoolVal $ a >= b
            (GreaterEq, FracNum a, FracNum b) -> BoolVal $ a >= b
            (GreaterEq, IntNum a, FracNum b) -> BoolVal $ (a % 1) >= b
            (GreaterEq, FracNum a, IntNum b) -> BoolVal $ a >= (b % 1)

            (_, _, _) -> Infix op lhs rhs
evaluate (RastorSet s) =
    RastorSet <$> foldl
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
evaluate (Applic EmptySet _) = error "cant apply on Null"
evaluate (Applic func ps) = do
    func' <- evaluate func
    evaluate $ Applic func' ps

evaluate (Proc stmt) = do
    res <- interpret stmt
    return EmptySet
evaluate (Ref name) = do
    ns <- getNames
    case lookupInScopes ns name of
        Nothing -> error $ "undefined name " ++ show name ++ " in scopes " ++ show ns
        Just (patt, expr) ->
            case patt of
                Pattern _ [] -> return expr
                Pattern _ ps -> return $ Func patt expr

evaluate (IfThenElse c tb fb) = do
    cond <- evaluate c
    if cond == BoolVal True then evaluate tb
    else if cond == BoolVal False then evaluate fb
    else error "if-then-else condition must be a boolean"

evaluate e = return e

interpretFull :: Statement -> IO Context
interpretFull stmt = do
    execStateT (interpret stmt) (Context { names = [] })

run :: String -> IO Context
run = interpretFull . parse