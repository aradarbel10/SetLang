module AST where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.List
import Data.Ratio
import Data.Tuple

data Var = Named String | Slack Integer
    deriving (Read, Eq, Ord)

instance Show Var where
    show (Named str) = str
    show (Slack i) = "s" ++ show i

type Scope = M.Map Pattern Expression
type Scopes = [Scope];
newtype Context = Context { names :: Scopes }
    deriving (Show)

data Statement = Nop | Echo
               | Block [Statement]
               | Definition Pattern Expression
               | Assign Pattern Expression
               | Exec Expression
               | IfStatement Expression Statement (Maybe Statement)
    deriving (Show, Eq, Ord)

funcDef :: Pattern -> Expression -> Statement
funcDef = Definition

procDef :: Pattern -> Statement -> Statement
procDef patt stmt = Definition patt $ Proc stmt

data Pattern = Pattern String [(String, Type)]
    deriving (Show, Eq, Ord)

data Type = Top | Bottom
          | BoolT | IntT | FracT
          | FuncT Type Type
    deriving (Show, Eq, Ord)

data Expression = EmptySet | Any | Nats | Ints | Reals | Ratios | Sets
                | RastorSet (S.Set Expression)
                | BoolVal Bool
                | StrVal String
                | IntNum Integer
                | FracNum Rational
                | Ref String
                | VarRef Var
                | Func Pattern Expression
                | Applic Expression [Expression]
                | IfThenElse Expression Expression Expression
                | Prefix PreOp Expression
                | Infix BinOp Expression Expression
                | Proc Statement
    deriving (Eq, Ord)


-- generic function to operate on nested expressions
-- iterates through every subexpression `e` of `expr`
-- applies on each `f => (e', a)`.
--
-- substitutes `e'` for `e`,
-- and accumulates `a`
mapFoldExpression' :: (a -> a -> a) -> a -> (a -> Expression -> (Expression, a)) -> Expression -> (Expression, a)
mapFoldExpression' append acc f expr = case expr of
    RastorSet set ->
        let lst = S.toList set
            (acc', es') = mapAccumL (\x y -> swap $ f x y) acc lst
        in  (RastorSet $ S.fromList es', acc')
    Func p expr ->
        let (e, a) = f acc expr
        in (Func p e, a)
    Applic e es ->
        let lst = (e:es)
            (acc', e':es') = mapAccumL (\x y -> swap $ f x y) acc lst
        in  (Applic e' es', acc')
    Prefix op expr ->
        let (e, a) = f acc expr
        in  (Prefix op e, a)
    Infix op e1 e2 ->
        let lst = [e1, e2]
            (acc', [e1', e2']) = mapAccumL (\x y -> swap $ f x y) acc lst
        in  (Infix op e1' e2', acc')
    IfThenElse {} -> error "not implemented"
    Proc {} -> error "not implemented"
    _ -> (expr, acc)

-- equivalent to foldMap
foldExpression' :: (a -> a -> a) -> a -> (a -> Expression -> a) -> Expression -> a
foldExpression' append empt f = snd . mapFoldExpression' append empt (\a e-> (EmptySet, f a e))

-- monoid versions
mapFoldExpression :: Monoid a => (a -> Expression -> (Expression, a)) -> Expression -> (Expression, a)
mapFoldExpression = mapFoldExpression' mappend mempty

foldExpression :: (Monoid a) => (a -> Expression -> a) -> Expression -> a
foldExpression = foldExpression' mappend mempty

mapExpression :: (Expression -> Expression) -> Expression -> Expression
mapExpression f = fst . mapFoldExpression (\_ e -> (f e, ()))



isProc :: Expression -> Bool
isProc (Proc _) = True
isProc _ = False

data PreOp = Empty | Even | Odd | Neg
           | Minus | Plus
    deriving (Enum, Eq, Ord)

data BinOp = Add | Sub | Mul | Div
           | Equals | NEquals
           | Less | Greater | LessEq | GreaterEq
           | BAnd | BOr | Member
           | Union | Intersect | Diff | Symdiff
    deriving (Enum, Eq, Ord)


instance Show PreOp where
    show op = case op of
        Empty -> "Empty?"
        Even -> "Even?"
        Odd -> "Odd?"
        Neg -> "Neg?"
        Minus -> "-"
        Plus -> "+"

instance Show BinOp where
    show op = case op of
        Add -> "+"
        Sub -> "-"
        Mul -> "×"
        Div -> "÷"
        Equals -> "="
        NEquals -> "≠"
        Less -> "<"
        Greater -> ">"
        LessEq -> "≤"
        GreaterEq -> "≥"
        BAnd -> "∧"
        BOr -> "∨"
        Member -> "∈"
        Union -> "∪"
        Intersect -> "∩"
        Diff -> "\\"
        Symdiff -> "Δ"

instance Show Expression where
    show expr = case expr of
        EmptySet -> "∅"
        Any -> "𝔸"
        Nats -> "ℕ"
        Ints -> "ℤ"
        Reals -> "ℝ"
        Ratios -> "ℚ"
        Sets -> "Sets"
        RastorSet set -> concat ["{", intercalate ", " $ map show $ S.toList set, "}"]
        BoolVal b -> show b
        StrVal str -> "\"" <> str <> "\""
        IntNum n -> show n
        FracNum rat -> "(" <> show (numerator rat) <> "/" <> show (denominator rat) <> ")"
        Ref s -> s
        VarRef var -> show var
        Func _ _ -> error "not implemented"
        Applic expr args -> concat ["(", show expr, " ", unwords (map show args), ")"]
        IfThenElse cond tb fb -> concat ["if ", show cond, " then ", show tb, " else ", show fb]
        Prefix op expr -> "(" <> show op <> show expr <> ")"
        Infix op lhs rhs -> concat ["(", show lhs, " ", show op, " ", show rhs, ")"]
        Proc _ -> error "not implemented"