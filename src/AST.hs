module AST where

import qualified Data.Set as S
import qualified Data.Map as M

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
    deriving (Show, Eq, Ord)

isProc :: Expression -> Bool
isProc (Proc _) = True
isProc _ = False

data PreOp = Empty | Even | Odd | Neg
           | Minus | Plus
    deriving (Show, Enum, Eq, Ord)

data BinOp = Add | Sub | Mul | Div
           | Equals | NEquals
           | Less | Greater | LessEq | GreaterEq
           | BAnd | BOr | Member
           | Union | Intersect | Diff | Symdiff
    deriving (Show, Enum, Eq, Ord)