module EUF where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Util

data Disjoints a = Disjoints { links :: M.Map Integer Integer, vals :: M.Map a Integer }
    deriving (Show, Eq, Ord)

(.==) = (!)
(./=) = (!)


systemUnsat :: ([(Expr, Expr)], [(Expr, Expr)])
systemCompose :: ([(Expr, Expr)], [(Expr, Expr)])

systemUnsat = ([
        func "g" [var "a"] .== var "c"
    ],[
        func "f" [func "g" [var "a"]] ./= func "f" [var "c"],
        var "c" ./= var "d"
    ])

-- f⁵(x) = x ⋀ f³(x) = x → f(x) = x
systemCompose = ([
        func "f" [func "f" [func "f" [func "f" [func "f" [var "x"]]]]] .== var "x",
        func "f" [func "f" [func "f" [var "x"]]] .== var "x"
    ],[
        func "f" [var "x"] ./= var "x"
    ])

isSat (eqs, neqs) = satisfiable eqs neqs


makeDisjoints :: Ord a => [a] -> Disjoints a
makeDisjoints = extendDisjoints $ Disjoints { links = M.empty, vals = M.empty }

extendDisjoints :: Ord a => Disjoints a -> [a] -> Disjoints a
extendDisjoints set xs = let newis = [toInteger $ M.size (links set) + 1 ..] in
    Disjoints { links = links set `M.union` M.fromList (take (length xs) (zip newis newis))
              , vals  = vals  set `M.union` M.fromList (zip xs newis) }

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
    Disjoints { links = M.adjust (const $ find' set ito) (find' set ifrom) (links set)
              , vals = vals set }

data Expr = Expr String [Expr]
    deriving (Read, Eq, Ord)

instance Show Expr where
    show (Expr name params) = name ++ (if null params
        then ""
        else "(" ++ L.intercalate ", " (map show params) ++ ")")

var :: String -> Expr
var name = Expr name []

func = Expr

children :: Expr -> [Expr]
children (Expr _ cs) = cs

funcname :: Expr -> String
funcname (Expr name _) = name

arity :: Expr -> Integer
arity (Expr _ ps) = toInteger $ length ps

congruent :: Disjoints Expr -> Expr -> Expr -> Bool
congruent set e1 e2 = funcname e1 == funcname e2
                && arity e1 == arity e2
                && and (zipWith (\u v -> (find set u == find set v) || congruent set u v) (children e1) (children e2))

allSubexprs :: Expr -> S.Set Expr
allSubexprs (Expr name params) = S.insert (Expr name params) (S.unions $ map allSubexprs params)

allCongruents :: Disjoints Expr -> Integer -> S.Set Integer
allCongruents set index = let rep = find' set index in
    M.keysSet $ M.filterWithKey (\k _ -> rep == find' set k) (links set)

parentExprs :: S.Set Expr -> Expr -> S.Set Expr
parentExprs exprs expr = S.filter (==expr) (S.fromList $ foldMap children exprs)

congruentParents :: Disjoints Expr -> Expr -> S.Set Expr
congruentParents set expr = let index = (M.!) (vals set) expr
                                congs = S.map (fromJust . getByIndex set) (allCongruents set index)
                            in  S.unions $ S.map (parentExprs (M.keysSet $ vals set)) congs

satisfiable :: [(Expr, Expr)] -> [(Expr, Expr)] -> Bool
satisfiable eqs neqs =
    let subes = S.toList $ S.unions $ map allSubexprs (unpair eqs ++ unpair neqs)
        set = foldl addEq (makeDisjoints subes) eqs
    in not $ any (checkEq set) neqs

addEq :: Disjoints Expr -> (Expr, Expr) -> Disjoints Expr
addEq set (lhs, rhs) = if find set lhs == find set rhs
    then set
    else let lps = congruentParents set lhs
             rps = congruentParents set rhs
             set' = union set lhs rhs
             merge s' (e1, e2) = if find s' e1 == find s' e2 && congruent set' e1 e2
                 then s'
                 else addEq s' (e1, e2)
    in foldl merge set' (S.cartesianProduct lps rps)

checkEq :: Disjoints Expr -> (Expr, Expr) -> Bool
checkEq set (lhs, rhs) = (find set lhs == find set rhs) || congruent set lhs rhs