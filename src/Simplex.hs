module Simplex where

import qualified Data.Map as M
import qualified Data.List as L

import Util
import Data.List (intercalate)


data Variable = Var String
              | Slack Integer
    deriving (Read, Eq, Ord)

instance Show Variable where
    show (Var name) = name
    show (Slack i) = "s" <> show i

type Value = Double
data Bound = Val Value | Min | Max
    deriving (Eq)

instance Show Bound where
    show (Val v) = show v
    show Min = "-infty"
    show Max = "+infty"

(.<) :: Value -> Bound -> Bool
v .< Max = True
v .< Min = False
v .< (Val b) = v < b

(.>) :: Value -> Bound -> Bool
v .> Max = False
v .> Min = True
v .> (Val b) = v > b

fromBound :: Bound -> Value
fromBound (Val v) = v
fromBound _ = error "non val"


data Operator = Eq | Leq | Geq
    deriving (Show, Read, Eq, Ord, Enum)

data Ineq = Ineq [Value] Operator Bound
    deriving (Show, Eq)
-- System of inequalities  normal vars
data System = System [Variable] [Ineq]

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

toNormalForm :: System -> NormalForm
toNormalForm (System vars ineqs) =
    let (coeffs, ops, vals) =
            unzip3 $ map (\(Ineq coeffs op val) -> (coeffs, op, val)) ineqs
        slacks = Slack <$> [1..]
    in Normal {
        table = M.fromList $ zip slacks (flip map coeffs $ \row ->
            M.fromList $ zip vars row),
        bounds = M.fromList (zip vars (repeat (Min, Max)))
            `M.union` M.fromList (zip slacks (zipWith opToBounds ops vals)),
        assgn = M.fromList $
            zip (take (length ineqs) slacks ++ vars) (repeat 0)
    }

exampleSystem :: System
exampleSystem = System
    [Var "x", Var "y"]
    [
        Ineq [1, 1] Leq (Val $ -4),
        Ineq [1,-1] Leq Max
    ]

data NormalForm = Normal { table :: M.Map Variable (M.Map Variable Value)
                         , bounds :: M.Map Variable (Bound, Bound)
                         , assgn :: M.Map Variable Value
                         }

instance Show NormalForm where
    show Normal {table = t, bounds = b, assgn = a} =
        unlines (map (\(free, row) -> show free <> " = " <>
            intercalate " + " (map (\(var, coeff) -> show coeff <> show var) $ M.toList row)) (M.toList t))
        <> "\n\n"
        <> unlines (map (\(var, (lo, hi)) -> intercalate " <= " [show lo, show var, show hi]) (M.toList b))
        <> "\n\n"
        <> unlines (map (\(var, val) -> show var <> " = " <> show val) (M.toList a))


findCoefficient :: NormalForm -> Variable -> Variable -> Maybe Value
findCoefficient form b n =
    let tab = table form
    in M.lookup b tab >>= M.lookup n

isBasic :: NormalForm -> Variable -> Bool
isBasic form var = M.member var (table form)

isOriginalVar :: Variable -> Bool
isOriginalVar (Var _) = True
isOriginalVar _       = False

pivotAndUpdate :: NormalForm -> Variable -> Variable -> Value -> NormalForm
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

update :: NormalForm -> Variable -> Value -> NormalForm
update form var val =
    let oldval = fromJust $ M.lookup var (assgn form)
        mapped = M.mapWithKey (\k v -> if isBasic form k
            then v + fromJust (findCoefficient form k var) * (val - oldval)
            else v) (assgn form)
    in form { assgn = M.adjust (const val) var mapped }


pivot :: NormalForm -> Variable -> Variable -> NormalForm
pivot form row col =
    let tabl = table form
        coeff = fromJust $ findCoefficient form row col
        subsrow = fromJust $ M.lookup row tabl
    in swap row col $ form { table = M.mapWithKey (\rowname eqn ->
        if rowname == row
            then pivotMainRow eqn col coeff
            else pivotOtherRow eqn col coeff subsrow) tabl }

swap :: Variable -> Variable -> NormalForm -> NormalForm
swap r c form =
    Normal {
        table = M.mapKeys (\key -> if key == r then c else key)
               (M.map (M.mapKeys (\cell -> if cell == c then r else cell)) (table form)),
        --bounds = M.mapKeys (\key -> if key == r then c else
        --                            if key == c then r else key) (bounds form),
        bounds = bounds form,
        assgn = assgn form
    }

pivotMainRow :: M.Map Variable Value -> Variable -> Value -> M.Map Variable Value
pivotMainRow row cell coeff =
    M.mapWithKey (\var v ->
        if var == cell
            then 1/coeff
            else -v/coeff) row

pivotOtherRow :: M.Map Variable Value -> Variable -> Value -> M.Map Variable Value -> M.Map Variable Value
pivotOtherRow row cell coeff subsrow =
    let koeff = fromJust $ M.lookup cell row
    in M.mapWithKey (\var v ->
        if var == cell
            then v/coeff
            else let t = fromJust $ M.lookup var subsrow
                 in v - (koeff * t) / coeff) row

minWith :: (k -> a -> Bool) -> M.Map k a -> Maybe (k, a)
minWith f m = M.lookupMin $ M.filterWithKey f m

check :: NormalForm -> Maybe (M.Map Variable Value)
check form =
    let chooseBasic' = minWith (\var val -> case M.lookup var (bounds form) of
                Nothing -> False
                Just (lo, hi) -> val .< lo || val .> hi) (assgn form)
    in case chooseBasic' of
        Nothing -> Just $ M.filterWithKey (const . isOriginalVar) (assgn form)
        Just (chooseBasic, basicVal) ->
            let (basicLo, basicHi) = fromJust $ M.lookup chooseBasic (bounds form)
                row = fromJust $ M.lookup chooseBasic (table form)
                chooseNonbasic' = minWith (\var val ->
                    let curr = fromJust $ M.lookup var (assgn form)
                        (lo, hi) = fromJust $ M.lookup var (bounds form)
                    in (val > 0 && curr .< hi) || (val < 0 && curr .> lo)) row
            in case chooseNonbasic' of
                Nothing -> Nothing
                Just (chooseNonbasic, coeff) ->
                    let result = pivotAndUpdate form chooseBasic chooseNonbasic (
                            if basicVal .< basicLo
                                then fromBound basicLo
                            else if basicVal .> basicHi
                                then fromBound basicHi
                            else error "unreachable")
                    in check result