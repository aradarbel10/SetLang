{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isNothing" #-}
{-# HLINT ignore "Use isJust" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Mather where

import Util

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Graph as G

import Data.Foldable
import Control.Monad.State.Lazy
import Data.Proxy
import Data.Functor



import qualified EUF
import qualified Simplex as Spx

import qualified AST
import AST(Var(Named, Slack))
import Data.Map (valid)
import Debug.Trace
debug = flip trace

data TheoryID = EUF | NUM | SET | SAT | STR | IMP
    deriving (Read, Show, Eq, Ord, Enum)

class Ord pred => Theory pred expr | pred -> expr, expr -> pred where
    theory      :: pred -> TheoryID
    convex      :: pred -> Bool
    equate      :: expr -> expr -> pred
    var         :: AST.Var -> expr
    convertPred :: AST.Expression -> Maybe pred
    convertExpr :: AST.Expression -> Maybe expr
    satisfiable :: S.Set pred -> Bool
 -- preprocess  :: S.Set term -> (AST.Var -> AST.Var -> Bool)


{-
things every theory needs:
- an ID of some sort
- whether it's convex or not
- the notion of
    - ground formulas
    - formal variables
    - equality formula
- convert AST -> theory raw form
- convert theory raw form -> theory normal form (internal)
- decision procedure :: [formula] -> some kind of result?
-}

instance Theory EUF.Pred EUF.Expr where
    theory _ = EUF
    convex _ = True
    equate lhs rhs = EUF.Eq lhs rhs
    var v = EUF.Expr v []
    convertPred = EUF.convertPred
    convertExpr = EUF.convertExpr
    satisfiable = EUF.satisfiable

instance Theory Spx.Pred Spx.Expr where
    theory _ = NUM
    convex _ = True
    equate lhs rhs = Spx.buildPred Spx.Eq lhs rhs
    var v = Spx.Expr $ M.singleton (Just v) 1
    convertPred = Spx.convertPred
    convertExpr = Spx.convertExpr
    satisfiable = isJust . Spx.satisfiable

data GroundTerm = forall p e. (Show p, Theory p e) => Gt p

showGt :: GroundTerm -> [Char]
showGt (Gt p) = show p

instance Show GroundTerm where
    show = showGt

data TAtom = Var String | Pure Integer
    deriving (Read, Show, Ord, Eq)

associateTheory :: AST.Expression -> TheoryID
associateTheory expr = case expr of
    AST.EmptySet -> SET
    AST.Any -> SET
    AST.Nats -> SET
    AST.Ints -> SET
    AST.Reals -> SET
    AST.Ratios -> SET
    AST.Sets -> SET
    AST.RastorSet _ -> SET
    AST.BoolVal _ -> SAT
    AST.StrVal _ -> STR
    AST.IntNum _ -> NUM
    AST.FracNum _ -> NUM
    AST.Ref _ -> EUF
    AST.VarRef _ -> EUF
    AST.Func _ _ -> EUF
    AST.Applic _ _ -> EUF
    AST.IfThenElse {} -> IMP
    AST.Prefix op expr -> case op of
        AST.Empty -> SET
        AST.Even -> NUM
        AST.Odd -> NUM
        AST.Neg -> NUM
        AST.Minus -> NUM
        AST.Plus -> NUM
    AST.Infix op e1 e2 -> case op of
        AST.Add -> NUM
        AST.Sub -> NUM
        AST.Mul -> NUM
        AST.Div -> NUM
        AST.Equals -> EUF
        AST.NEquals -> EUF
        AST.Less -> NUM
        AST.Greater -> NUM
        AST.LessEq -> NUM
        AST.GreaterEq -> NUM
        AST.BAnd -> SAT
        AST.BOr -> SAT
        AST.Member -> SET
        AST.Union -> SET
        AST.Intersect -> SET
        AST.Diff -> SET
        AST.Symdiff -> SET
    AST.Proc state -> IMP
    _ -> error "unhandled"

sameTheory :: AST.Expression -> AST.Expression -> Bool
sameTheory (AST.Ref _) _ = True
sameTheory _ (AST.Ref _) = True
sameTheory (AST.VarRef _) _ = True
sameTheory _ (AST.VarRef _) = True
sameTheory e1 e2 = associateTheory e1 == associateTheory e2

isTheoryOf :: TheoryID -> AST.Expression -> Bool
isTheoryOf _ (AST.Ref _) = True
isTheoryOf _ (AST.VarRef _) = True
isTheoryOf t e = t == associateTheory e


-- a purification context remembers all of the equalities
-- generated during putification
data PurifContext =
    Purif {
        sets :: M.Map TheoryID [AST.Expression],
        slacks :: Integer
    }
    deriving (Show)

emptyPurif = Purif { sets = M.empty, slacks = 0 }

addTerm :: AST.Expression -> State PurifContext ()
addTerm expr = state $ \purif ->
    ((), purif {
        sets = M.adjust (expr:) (associateTheory expr) (sets purif)
    })

addEq :: AST.Expression -> AST.Expression -> State PurifContext ()
addEq lhs rhs = addTerm (AST.Infix AST.Equals lhs rhs)

fresh :: State PurifContext AST.Var
fresh = state $ \purif ->
    (AST.Slack $ slacks purif, purif { slacks = slacks purif + 1 })

-- substitute the expression with a fresh slack variable
-- and add their equality to the purification context
psubst :: TheoryID -> AST.Expression -> State PurifContext AST.Expression
psubst t expr = if t `isTheoryOf` expr
    then return expr
    else do
        v <- AST.VarRef <$> fresh
        addEq v expr
        return v

purifyExpr :: AST.Expression -> Maybe (State PurifContext AST.Expression)
purifyExpr expr = let thry = associateTheory expr in case expr of
    AST.Infix op lhs rhs ->
        Just $ do
            lhs' <- psubst thry lhs
            rhs' <- psubst thry rhs
            return $ AST.Infix op lhs' rhs'
    AST.Prefix op rhs ->
        Just $ do
            rhs' <- psubst thry rhs
            return $ AST.Prefix op rhs'
    AST.Applic func args ->
        Just $ do
            curr <- get :: State PurifContext PurifContext
            args' <-
                let substed = map (psubst thry) args
                in  sequence substed
            return $ AST.Applic func args'
    _ -> Nothing

{-
purify :: AST.Expression -> State PurifContext AST.Expression
purify expr = case expr of
    AST.Infix op e1 e2 ->
        if     op == AST.Add
            || op == AST.Sub
            || op == AST.Mul
        then do
            e1' <- purify e1 >>= psubstSpx
            e2' <- purify e2 >>= psubstSpx
            return $ AST.Infix op e1' e2'
        else case op of
        AST.Div -> error "unsupported nonlinear arithmetic"
        AST.Equals -> do
            e1' <- purify e1 >>= psubstEuf
            e2' <- purify e2 >>= psubstEuf
            addTerm $ Gt (fromJust $ convertPred (AST.Infix AST.Equals e1' e2') :: EUF.Pred)
            return $ AST.Infix AST.Equals e1' e2'
        AST.NEquals -> error "unsupported nonequalities"
        AST.Less -> error "unsupported strict inequalities"
        AST.Greater -> error "unsupported strict inequalities"
        AST.LessEq ->
            let expr' = convertPred expr
            in do
                when (isJust expr')
                    (addTerm $ Gt (fromJust expr' :: Spx.Pred))
                return expr
        AST.GreaterEq ->
            let expr' = convertPred expr
            in do
                when (isJust expr')
                    (addTerm $ Gt (fromJust expr' :: Spx.Pred))
                return expr
        AST.BAnd -> error "unsupported boolean constraint"
        AST.BOr -> error "unsupported boolean constraint"
        AST.Member -> error "unsupported set constraint"
        AST.Union -> error "unsupported set constraint"
        AST.Intersect -> error "unsupported set constraint"
        AST.Diff -> error "unsupported set constraint"
        AST.Symdiff -> error "unsupported set constraint"
        _ -> error "unreachable"
    AST.Proc state -> error "unsupported imperative constraint"
    _ -> return expr
-}

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
        And es -> "(" ++ unwords (L.intersperse "∧" $ map show es) ++ ")"
        Or  es -> "("  ++ unwords (L.intersperse "∨" $ map show es) ++ ")"
        Not e -> "¬" ++ show e
        e1 :=> e2 -> "(" ++ show e1 ++ " ⇒  " ++ show e2 ++ ")"
        e1 :<=> e2 -> "(" ++ show e1 ++ " ⇔ " ++ show e2 ++ ")"

isAnd :: Expr -> Bool
isAnd (And _) = True
isAnd _ = False

isOr :: Expr -> Bool
isOr (Or _) = True
isOr _ = False


type Literal = (String, Bool)
type Clause = S.Set Literal
type CNF = S.Set Clause

toLiteral :: Expr -> Literal
toLiteral expr = case expr of
    Not (Atom s) -> (s, False)
    Atom s -> (s, True)
    _ -> error "non CNF"

toClause :: Expr -> Clause
toClause expr = case expr of
    Or ls -> S.fromList $ map toLiteral ls
    other -> S.singleton $ toLiteral other

fromList :: [[Expr]] -> CNF
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
toNNF (e1 :=> e2)  = Or [toNNF $ Not e1, toNNF e2]
toNNF (e1 :<=> e2) = Or [And [toNNF e1, toNNF e2], And [Not $ toNNF e1, Not $ toNNF e2]]
-- propagate nagation
toNNF (Not expr) = case expr of
    BTrue  -> BTrue
    BFalse -> BFalse
    Atom _ -> Not expr
    And es -> toNNF $ Or  $ map Not es
    Or es  -> toNNF $ And $ map Not es
    Not e  -> toNNF e
    implic -> toNNF $ Not (toNNF implic)
toNNF (And es) = And $ map toNNF es
toNNF (Or  es) = Or  $ map toNNF es
toNNF other = other -- leave as-is

isNNF :: Expr -> Bool
isNNF expr = toNNF expr == expr

-- Conjunction Normal Form:
-- (... or ...) and (... or ...) and ... and (... or ...)
toCNF :: Expr -> CNF
toCNF = exprToCNF . distribute . toNNF

isCNF :: Expr -> Bool
isCNF expr = (distribute . toNNF) expr == expr


exprToCNF :: Expr -> CNF
exprToCNF e = case e of
    (And es) -> S.fromList $ map toClause es
    (Or es) -> S.singleton $ S.fromList $ map toLiteral es
    other -> S.singleton $ S.singleton $ toLiteral other

findUnitClause :: CNF -> Maybe Literal
findUnitClause cnf = case S.lookupMin cnf of
    Nothing -> Nothing
    Just clause ->
        if S.size clause == 1
            then S.lookupMin clause
            else Nothing

assignUnit :: CNF -> Literal -> CNF
assignUnit cnf lit = S.map (S.filter ((/=) (fst lit) . fst)) $ S.filter (not . S.member lit) cnf

unitPropagate :: (CNF, Solution) -> Maybe (CNF, Solution)
unitPropagate (cnf, sol) = case findUnitClause cnf of
    Nothing -> Nothing
    Just (atom, sgn) -> Just (assignUnit cnf (atom, sgn),
                              M.union sol (M.singleton atom sgn))

chooseLiteral :: CNF -> Maybe Literal
chooseLiteral cnf = S.lookupMin cnf >>= S.lookupMin

pureElimination :: (CNF, Solution) -> Maybe (CNF, Solution)
pureElimination (cnf, sol) =
    let uni = S.unions cnf
        pures = S.foldl (\acc (atom, sgn) ->
            if S.member (atom, not sgn) acc
                then S.delete (atom, not sgn) acc
                else S.insert (atom, sgn) acc) S.empty uni
    in if S.null pures
        then Nothing
        else Just (S.filter (S.null . S.intersection pures) cnf,
                   M.union sol (M.fromList $ S.toList pures))

dpllSAT' :: Solution -> CNF -> Maybe Solution
dpllSAT' sol cnf =
    let (reduced, newsol) = (untilNothing pureElimination . untilNothing unitPropagate) (cnf, sol)
    in if S.null reduced
        then Just newsol
    else if S.member S.empty reduced
        then Nothing
    else case chooseLiteral reduced of
        Nothing -> error "unreachable"
        Just (atom, _) ->
            case dpllSAT' (M.insert atom True newsol) (assignUnit reduced (atom, True)) of
            Just sol' -> Just sol'
            Nothing -> dpllSAT' (M.insert atom False newsol) (assignUnit reduced (atom, False))

dpllSAT :: Expr -> Maybe Solution
dpllSAT = dpllSAT' M.empty . toCNF

provable :: Expr -> Bool
provable = (==Nothing) . dpllSAT . Not