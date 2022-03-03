{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
{-# HLINT ignore "Use isJust" #-}
module Mather where

import Util

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Data.Foldable

import Debug.Trace
import AST (Expression)
debug = flip trace

data Expr = BTrue | BFalse
          | Atom String
          | And [Expr]
          | Or [Expr]
          | Not Expr
          | Expr :=> Expr
          | Expr :<=> Expr
    deriving (Read, Ord)

instance Eq Expr where
    (==) e1 e2 = case (e1, e2) of
        (BTrue, BTrue)              -> True
        (BFalse, BFalse)            -> True
        (Atom a1, Atom a2)          -> a1 == a2
        (And es1, And es2)          -> S.fromList es1 == S.fromList es2
        (Or es1, Or es2)            -> S.fromList es1 == S.fromList es2
        (Not e1, Not e2)            -> e1 == e2
        (p1 :=> q1, p2 :=> q2)      -> p1 == p2 && q1 == q2
        (p1 :<=> q1, p2 :<=> q2)    -> (p1 == p2 && q1 == q2) || (p1 == q2 && q1 == p2)
        (_, _)                      -> False

instance Show Expr where
    show expr = case expr of
        BTrue -> "true"
        BFalse -> "false"
        Atom str -> str
        And es -> "(and " ++ unwords (map show es) ++ ")"
        Or  es -> "(or "  ++ unwords (map show es) ++ ")"
        Not e -> "(not " ++ show e ++ ")"
        e1 :=> e2 -> "(" ++ show e1 ++ " -> " ++ show e2 ++ ")"
        e1 :<=> e2 -> "(" ++ show e1 ++ " <-> " ++ show e2 ++ ")"

isAnd :: Expr -> Bool
isAnd (And _) = True
isAnd _ = False

isOr :: Expr -> Bool
isOr (Or _) = True
isOr _ = False


data Literal a = Literal { atom :: a, pos :: Bool, neg :: Bool }
    deriving (Read, Show, Eq, Ord)
type Clause a = S.Set (Literal a)
type CNF a = S.Set (Clause a)

toLiteral :: Expr -> Literal String
toLiteral expr = case expr of
    Not (Atom s) -> Literal { atom = s, pos = False, neg = True }
    Atom s -> Literal { atom = s, pos = True, neg = False }
    _ -> error "non CNF"

toClause :: Expr -> Clause String
toClause expr = case expr of
    Or ls -> S.fromList $ map toLiteral ls
    other -> S.singleton $ toLiteral other

fromList :: [[Expr]] -> CNF String
fromList = S.fromList . map (S.fromList . map toLiteral)



-- DFS for the first Atom in the expression
findAtom :: Expr -> Maybe String
findAtom expr = case expr of
    BTrue       -> Nothing
    BFalse      -> Nothing
    Atom atom   -> Just atom
    And es      -> firstMaybe findAtom es
    Or es       -> firstMaybe findAtom es
    Not e       -> findAtom e
    e1 :=> e2   -> findAtom e1 `justOr` findAtom e2
    e1 :<=> e2  -> findAtom e1 `justOr` findAtom e2

evaluate :: Expr -> Bool
evaluate expr = case expr of
    BTrue       -> True
    BFalse      -> False
    Atom _      -> error "unexpected atom"
    And es      -> all evaluate es
    Or es       -> any evaluate es
    Not e       -> not $ evaluate e
    e1 :=> e2   -> not (evaluate e1) || evaluate e2
    e1 :<=> e2  -> evaluate e1 == evaluate e2

substitute :: Expr -> String -> Expr -> Expr
substitute newval name expr = case expr of
    BTrue       -> BTrue
    BFalse      -> BFalse
    Atom str    -> if str == name then newval
                                  else Atom str
    And es      -> And $ map (substitute newval name) es
    Or es       -> Or  $ map (substitute newval name) es
    Not e       -> Not $ substitute newval name e
    e1 :=> e2   -> substitute newval name e1 :=> substitute newval name e2
    e1 :<=> e2  -> substitute newval name e1 :<=> substitute newval name e2

-- a solution tells which value you substitute in each name
-- to satisfy the given expression
type Solution = M.Map String Bool

-- explores entire solution place until finds satisfying substitution
-- if can't satisfy, returns Nothing
bruteForceSAT :: Expr -> Maybe Solution
bruteForceSAT expr = case findAtom expr of
    Nothing ->
        if evaluate expr
            then Just M.empty
            else Nothing
    Just atom -> case bruteForceSAT $ substitute BTrue atom expr of -- try substituting true
        Just sat -> Just $ M.insert atom True sat
        Nothing -> case bruteForceSAT $ substitute BFalse atom expr of -- try substituting false
            Just sat -> Just $ M.insert atom False sat
            Nothing -> Nothing

-- merge nested operations of the same type
flatten' :: Expr -> Expr
flatten' (And []) = And []
flatten' (And (BTrue:es)) = flatten' (And es)
flatten' (And (BFalse:_)) = And [BFalse]
flatten' (And ((And nested):es)) = let (And es') = flatten' (And es) in And (nested ++ es')
flatten' (And (e:es)) = let (And es') = flatten' (And es) in And (e : es')

flatten' (Or []) = Or []
flatten' (Or (BTrue:_)) = Or [BTrue]
flatten' (Or (BFalse:es)) = flatten' (Or es)
flatten' (Or ((Or nested):es)) = let (Or es') = flatten' (Or es) in Or (nested ++ es')
flatten' (Or (e:es)) = let (Or es') = flatten' (Or es) in Or (e : es')

flatten' e = e -- leave as-is

raiseSingle :: Expr -> Expr
raiseSingle (And [e]) = e
raiseSingle (Or  [e]) = e
raiseSingle e = e -- leave as-is


flatten :: Expr -> Expr
flatten = raiseSingle . flatten'


-- use distributivity law to convert OR of ANDs to AND of ORs
-- (might blow exponentially, but necessary for CNF)
distribute :: Expr -> Expr
distribute expr = case expr of
    (Or es) ->
        if find isOr es /= Nothing
            then distribute $ flatten (Or es)
            else if find isAnd es /= Nothing
                then let (ands, rest) = applyFst (map $ \(And fs) -> fs) $ L.partition isAnd es
                         choices = chooseOnes ands
                     in distribute $ And (map (Or . (++rest)) choices)
                else Or (map distribute es)
    (And es) -> let es' = map distribute es in flatten (And es')
    _ -> expr -- leave as-is
    
    

-- Negation Normal Form:
-- negation can only appear on atoms
toNNF :: Expr -> Expr
-- eliminate implication
toNNF (e1 :=> e2)  = Or [Not $ toNNF e1, toNNF e2]
toNNF (e1 :<=> e2) = Or [And [toNNF e1, toNNF e2], And [Not $ toNNF e1, Not $ toNNF e2]]
-- propagate nagation
toNNF (Not expr) = case expr of
    BTrue  -> BTrue
    BFalse -> BFalse
    Atom _ -> Not expr
    And es -> Or  $ map (toNNF . Not) es
    Or es  -> And $ map (toNNF . Not) es
    Not e  -> toNNF e
    implic -> toNNF (Not $ toNNF implic)
toNNF other = other -- leave as-is

isNNF :: Expr -> Bool
isNNF expr = toNNF expr == expr

-- Conjunction Normal Form:
-- (... or ...) and (... or ...) and ... and (... or ...)
toCNF :: Expr -> CNF String
toCNF = exprToCNF . distribute . toNNF

isCNF :: Expr -> Bool
isCNF expr = (distribute . toNNF) expr == expr


exprToCNF :: Expr -> CNF String
exprToCNF e = case e of
    (And es) -> S.fromList $ map toClause es
    (Or es) -> S.singleton $ S.fromList $ map toLiteral es
    other -> S.singleton $ S.singleton $ toLiteral other


-- check whether a sentence holds
holds :: Expr -> Bool
holds sentence = bruteForceSAT (Not sentence) == Nothing