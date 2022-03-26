module Simplex where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Control.Applicative ( Applicative(liftA2) )

import Util
import Data.List (intercalate)
import qualified AST
import AST (Var(Named, Slack))

import Debug.Trace
import Distribution.Compat.Lens (_1)
debug = flip trace

type Value = Double
data Bound = Val Value | Min | Max
    deriving (Read, Eq, Ord)
type Assignment = M.Map AST.Var Value

instance Show Bound where
    show (Val v) = show v
    show Min = "-infty"
    show Max = "+infty"

(.<?) :: Value -> Bound -> Bool
v .<? Max = True
v .<? Min = False
v .<? (Val b) = v < b

(.>?) :: Value -> Bound -> Bool
v .>? Max = False
v .>? Min = True
v .>? (Val b) = v > b

fromBound :: Bound -> Value
fromBound (Val v) = v
fromBound _ = error "non val"

data Operator = Eq | Leq | Geq
    deriving (Show, Read, Eq, Ord, Enum)

(.==) :: M.Map AST.Var Value -> Bound -> Pred
(.==) a = Pred a Eq

(.<=) :: M.Map AST.Var Value -> Bound -> Pred
(.<=) a = Pred a Leq

(.>=) :: M.Map AST.Var Value -> Bound -> Pred
(.>=) a = Pred a Geq


data Pred = Pred (M.Map AST.Var Value) Operator Bound
    deriving (Read, Show, Eq, Ord)

newtype Expr = Expr (M.Map (Maybe AST.Var) Value)
    deriving (Read, Show, Eq, Ord)

fromExpr :: Expr -> M.Map (Maybe Var) Value
fromExpr (Expr expr) = expr

buildPred :: Operator -> Expr -> Expr -> Pred
buildPred op (Expr lhs) (Expr rhs) =
    let (vars, free) = M.partitionWithKey (\k _ -> isJust k) (M.unionWith (-) lhs rhs)
        row = M.mapKeys fromJust vars
    in  fromJust $ Pred row op <$> Just ( Val $
        if M.null free
            then 0
            else ((-1)*) $ snd $ M.findMin free
        )

convertPred :: AST.Expression -> Maybe Pred
convertPred ast = case ast of
    AST.Infix op lhs rhs ->
        let op' = case op of
                AST.Equals    -> Just Eq
                AST.LessEq    -> Just Leq
                AST.GreaterEq -> Just Geq
                _             -> Nothing
            lhs' = convertExpr lhs
            rhs' = convertExpr rhs
        in  buildPred <$> op' <*> lhs' <*> rhs'
    _ -> Nothing

convertExpr :: AST.Expression -> Maybe Expr
convertExpr ast = case ast of
    AST.Ref name -> Just $ Expr $ M.singleton (Just $ Named name) 1
    AST.FracNum num -> Just $ Expr $ M.singleton Nothing (fromRational num)
    AST.Infix op lhs rhs ->
        let [lhs', rhs'] = L.sort [convertExpr lhs, convertExpr rhs]
        in  if isJust lhs' && isJust rhs'
                then case op of
                    AST.Add -> Expr <$> (M.unionWith (+) <$> (fromExpr <$> lhs') <*> (fromExpr <$> rhs'))
                    AST.Sub -> Expr <$> (M.unionWith (-) <$> (fromExpr <$> lhs') <*> (fromExpr <$> rhs'))
                    AST.Mul -> case M.toList $ fromExpr $ fromJust lhs' of
                        [(Nothing, num)] -> Just $ Expr $ M.map (+num) (fromExpr $ fromJust rhs')
                        _ -> Nothing
                    _ -> Nothing
                else Nothing
    AST.Prefix op expr ->
        let expr' = convertExpr expr
        in  if isJust expr'
            then case op of
                AST.Plus -> expr'
                AST.Minus -> Just $ Expr $ M.map (*(-1)) (fromExpr $ fromJust expr')
                _ -> Nothing
            else Nothing
    _ -> Nothing


getVars :: Pred -> S.Set AST.Var
getVars (Pred row _ _) = M.keysSet row

opToBounds :: Operator -> Bound -> (Bound, Bound)
opToBounds op val = case val of
    Val val -> case op of
        Eq ->  (Val val, Val val)
        Leq -> (Min    , Val val)
        Geq -> (Val val, Max    )
    Min -> case op of
        Eq ->  error "can't equal -infty"
        Leq -> error "can't be smaller than -infty"
        Geq -> (Min, Max)
    Max -> case op of
        Eq ->  error "can't equal +infty"
        Leq -> (Min, Max)
        Geq -> error "can't be greater than +infty"

toNormalForm :: S.Set Pred -> NormalForm
toNormalForm system =
    let (coeffs, ops, vals) =
            unzip3 $ S.toList $ S.map (\(Pred coeffs op val) -> (coeffs, op, val)) system
        slacks = AST.Slack <$> [1..]
        vars = S.toList $ S.unions $ S.map getVars system
    in Normal {
        table = M.fromList $ zip slacks coeffs,
        bounds = M.fromList (zip vars (repeat (Min, Max)))
            `M.union` M.fromList (zip slacks (zipWith opToBounds ops vals)),
        assgn = M.fromList $
            zip (take (S.size system) slacks ++ vars) (repeat 0)
    }

exampleSat :: S.Set Pred
exampleSat = S.fromList
    [ M.fromList [(Named "x",  2), (Named "y",  1)] .>= Val 5
    , M.fromList [(Named "x",  1), (Named "y", -1)] .<= Val 6
    , M.fromList [(Named "x", -2), (Named "y", -4)] .>= Val (-4)
    ]

exampleUnsat :: S.Set Pred
exampleUnsat = S.fromList
    [ M.fromList [(Named "x", 3), (Named "y", -1)] .>= Val 1
    , M.fromList [(Named "x", 2), (Named "y",  2)] .<= Val (-2)
    , M.fromList [(Named "x", 1), (Named "y", -2)] .<= Val (-2)
    ]


data NormalForm = Normal { table  :: M.Map AST.Var Assignment
                         , bounds :: M.Map AST.Var (Bound, Bound)
                         , assgn  :: M.Map AST.Var Value }

instance Show NormalForm where
    show Normal {table = t, bounds = b, assgn = a} =
        unlines (map (\(free, row) -> show free <> " = " <>
            intercalate " + " (map (\(var, coeff) -> show coeff <> show var) $ M.toList row)) (M.toList t))
        <> "\n\n"
        <> unlines (map (\(var, (lo, hi)) -> intercalate " <= " [show lo, show var, show hi]) (M.toList b))
        <> "\n\n"
        <> unlines (map (\(var, val) -> show var <> " = " <> show val) (M.toList a))


findCoefficient :: NormalForm -> AST.Var -> AST.Var -> Maybe Value
findCoefficient form b n =
    let tab = table form
    in M.lookup b tab >>= M.lookup n

isBasic :: NormalForm -> AST.Var -> Bool
isBasic form var = M.member var (table form)

isOriginalVar :: AST.Var -> Bool
isOriginalVar (AST.Named _) = True
isOriginalVar _       = False

pivotAndUpdate :: NormalForm -> AST.Var -> AST.Var -> Value -> NormalForm
pivotAndUpdate form bvar nvar val =
    case M.lookup bvar (assgn form) of
        Nothing -> error "variable doesnt exist"
        Just oldval -> let adj = (val - oldval) / fromJust (findCoefficient form bvar nvar)
                           form' = form { assgn =
                                M.mapWithKey (\var v ->
                                    if var == bvar
                                        then val
                                    else if var == nvar
                                        then v + adj
                                    else if isBasic form var
                                        then v + adj * fromJust (findCoefficient form var nvar)
                                    else v) (assgn form)
                                }
                       in pivot form' bvar nvar

update :: NormalForm -> AST.Var -> Value -> NormalForm
update form var val =
    let oldval = fromJust $ M.lookup var (assgn form)
        mapped = M.mapWithKey (\k v -> if isBasic form k
            then v + fromJust (findCoefficient form k var) * (val - oldval)
            else v) (assgn form)
    in form { assgn = M.adjust (const val) var mapped }


pivot :: NormalForm -> AST.Var -> AST.Var -> NormalForm
pivot form row col =
    let tabl = table form
        coeff = fromJust $ findCoefficient form row col
        subsrow = fromJust $ M.lookup row tabl
    in swap row col $ form { table = M.mapWithKey (\rowname eqn ->
        if rowname == row
            then pivotMainPred eqn col coeff
            else pivotOtherPred eqn col coeff subsrow) tabl }

swap :: AST.Var -> AST.Var -> NormalForm -> NormalForm
swap r c form =
    Normal {
        table = M.mapKeys (\key -> if key == r then c else key)
               (M.map (M.mapKeys (\cell -> if cell == c then r else cell)) (table form)),
        --bounds = M.mapKeys (\key -> if key == r then c else
        --                            if key == c then r else key) (bounds form),
        bounds = bounds form,
        assgn = assgn form
    }

pivotMainPred :: M.Map AST.Var Value -> AST.Var -> Value -> M.Map AST.Var Value
pivotMainPred row cell coeff =
    M.mapWithKey (\var v ->
        if var == cell
            then 1/coeff
            else -v/coeff) row

pivotOtherPred :: M.Map AST.Var Value -> AST.Var -> Value -> M.Map AST.Var Value -> M.Map AST.Var Value
pivotOtherPred row cell coeff subsrow =
    let koeff = fromJust $ M.lookup cell row
    in M.mapWithKey (\var v ->
        if var == cell
            then v/coeff
            else let t = fromJust $ M.lookup var subsrow
                 in v - (koeff * t) / coeff) row

minWith :: (k -> a -> Maybe x) -> M.Map k a -> Maybe (k, a, x)
minWith f m = case M.lookupMin $ M.filterWithKey (\k v -> isJust $ f k v) m of
    Nothing -> Nothing
    Just (k, v) -> Just (k, v, fromJust $ f k v)

check :: NormalForm -> Maybe Assignment
check form =
    let chooseBasic' = minWith (\var val -> case M.lookup var (bounds form) of
                Nothing -> Nothing
                Just (lo, hi) ->
                    if val .<? lo
                        then Just True  -- encode <lo
                    else if val .>? hi
                        then Just False -- encode >hi
                    else Nothing
                ) (assgn form)
    in case chooseBasic' of
        Nothing -> Just $ M.filterWithKey (const . isOriginalVar) (assgn form)
        Just (chooseBasic, basicVal, side) ->
            let (basicLo, basicHi) = fromJust $ M.lookup chooseBasic (bounds form)
                row = fromJust $ M.lookup chooseBasic (table form)
                compare' = if side then (>) else (<) -- choose direction based on sise
                chooseNonbasic' = minWith (\var val ->
                    let curr = fromJust $ M.lookup var (assgn form)
                        (lo, hi) = fromJust $ M.lookup var (bounds form)
                    in if (compare' val 0 && curr .<? hi) || (compare' 0 val && curr .>? lo)
                        then Just ()
                        else Nothing) row
            in case chooseNonbasic' of
                Nothing -> Nothing
                Just (chooseNonbasic, coeff, _) ->
                    let result = pivotAndUpdate form chooseBasic chooseNonbasic (
                            if basicVal .<? basicLo
                                then fromBound basicLo
                            else if basicVal .>? basicHi
                                then fromBound basicHi
                            else error "unreachable")
                    in check result

satisfiable :: S.Set Pred -> Maybe Assignment
satisfiable = check . toNormalForm