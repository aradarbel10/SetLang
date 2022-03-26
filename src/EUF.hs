{-# LANGUAGE LambdaCase #-}
module EUF where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import AST (Var(Named, Slack))
import qualified AST

import Util

data Disjoints a = Disjoints
    { links :: M.Map Integer Integer
    , vals :: M.Map a Integer
    , neqs :: S.Set (Expr, Expr) } deriving (Show, Eq, Ord)

data Pred = Eq  Expr Expr
          | Neq Expr Expr
    deriving (Read, Eq, Ord)

data Expr = Expr Var [Expr]
    deriving (Read, Eq, Ord)

instance Show Expr where
    show (Expr name params) = show name ++ (if null params
        then ""
        else "(" ++ L.intercalate ", " (map show params) ++ ")")

instance Show Pred where
    show (Eq  lhs rhs) = show lhs ++ " = " ++ show rhs
    show (Neq lhs rhs) = show lhs ++ " ≠ " ++ show rhs

convertPred :: AST.Expression -> Maybe Pred
convertPred (AST.Infix op lhs rhs) = Eq <$> convertExpr lhs <*> convertExpr rhs
convertPred _ = Nothing

rotateApplic :: AST.Expression -> AST.Expression
rotateApplic (AST.Applic (AST.Applic f as) bs) = rotateApplic $ AST.Applic f (as ++ bs)
rotateApplic e = e

convertExpr :: AST.Expression -> Maybe Expr
convertExpr expr = case expr of
    AST.Ref str -> Just $ var str
    AST.Applic f args ->
        let AST.Applic f args = rotateApplic expr
        in case f of
            AST.Ref name -> Just (Expr (Named name) (fromJust . convertExpr <$> args))
            _ -> Nothing
    _ -> Nothing

isEquality :: Pred -> Bool
isEquality (Eq _ _) = True
isEquality (Neq _ _) = False

makeDisjoints :: Ord a => [a] -> Disjoints a
makeDisjoints = extendDisjoints $ Disjoints
    { links = M.empty
    , vals = M.empty
    , neqs = S.empty }

extendDisjoints :: Ord a => Disjoints a -> [a] -> Disjoints a
extendDisjoints set xs = let newis = [toInteger $ M.size (links set) + 1 ..] in
    Disjoints { links = links set `M.union` M.fromList (take (length xs) (zip newis newis))
              , vals  = vals  set `M.union` M.fromList (zip xs newis)
              , neqs  = neqs  set }

getByIndex :: Disjoints a -> Integer -> Maybe a
getByIndex set index = let matches = M.filter (==index) (vals set) in
    if M.null matches
        then Nothing
        else Just $ fst $ M.elemAt 0 matches

find' :: Ord a => Disjoints a -> Integer -> Integer
find' set index = let parent = fromJust $ M.lookup index (links set)
    in if parent == index
        then index
        else find' set parent

find :: Ord a => Disjoints a -> a -> Integer
find set elem = find' set $ fromJust (M.lookup elem (vals set))

union :: Ord a => Disjoints a -> a -> a -> Disjoints a
union set from to = let ifrom = fromJust $ M.lookup from (vals set)
                        ito   = fromJust $ M.lookup to   (vals set) in
    set { links = M.adjust (const $ find' set ito) (find' set ifrom) (links set) }

var :: String -> Expr
var name = Expr (Named name) []

func :: String -> [Expr] -> Expr
func name = Expr (Named name)

children :: Expr -> [Expr]
children (Expr _ cs) = cs

funcname :: Expr -> Var
funcname (Expr name _) = name

arity :: Expr -> Integer
arity (Expr _ ps) = toInteger $ length ps

congruent :: Disjoints Expr -> Expr -> Expr -> Bool
congruent set e1 e2 = funcname e1 == funcname e2
                && arity e1 == arity e2
                && and (zipWith (\u v -> find set u == find set v) (children e1) (children e2))

allSubexprs :: Expr -> S.Set Expr
allSubexprs (Expr name params) = S.insert (Expr name params) (S.unions $ map allSubexprs params)

allCongruents :: Disjoints Expr -> Integer -> S.Set Integer
allCongruents set index = let rep = find' set index in
    M.keysSet $ M.filterWithKey (\k _ -> rep == find' set k) (links set)

parentExprs :: S.Set Expr -> Expr -> S.Set Expr
parentExprs exprs expr = S.filter (elem expr . children) exprs
    
congruentParents :: Disjoints Expr -> Expr -> S.Set Expr
congruentParents set expr = let index = (M.!) (vals set) expr
                                congs = S.map (fromJust . getByIndex set) (allCongruents set index)
                            in  S.unions $ S.map (parentExprs (M.keysSet $ vals set)) congs

satisfiable :: S.Set Pred -> Bool
satisfiable fs =
    let (eqs', neqs') = S.partition isEquality fs
        subes = S.toList $ S.unions $ S.map allSubexprs $ S.union (unpairPred eqs') (unpairPred neqs')
        set = foldl addEq (makeDisjoints subes) fs
    in not $ any (checkEq set) (neqs set)

unpairPred :: S.Set Pred -> S.Set Expr
unpairPred set = S.unions $ S.map (\case
    Eq lhs rhs -> S.fromList [lhs, rhs]
    Neq lhs rhs -> S.fromList [lhs, rhs]) set

addEq :: Disjoints Expr -> Pred -> Disjoints Expr
addEq set formula = case formula of
    Eq lhs rhs -> if find set lhs == find set rhs
        then set
        else let lps = congruentParents set lhs
                 rps = congruentParents set rhs
                 set' = union set lhs rhs
                 merge s' (e1, e2) = if find s' e1 /= find s' e2 && congruent set' e1 e2
                     then addEq s' (Eq e1 e2)
                     else s'
            in foldl merge set' (S.cartesianProduct lps rps)
    Neq lhs rhs -> set { neqs = S.insert (lhs, rhs) (neqs set) }

checkEq :: Disjoints Expr -> (Expr, Expr) -> Bool
checkEq set (lhs, rhs) = (find set lhs == find set rhs) || congruent set lhs rhs