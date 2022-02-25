module AST where

import qualified Data.Set as S
import qualified Data.Map as M


type Scope = M.Map Pattern Expression
type Scopes = [Scope];
data Context = Context {
                        names :: Scopes,
                        stdout :: String
                       }
    deriving (Show)

data Statement = Nop
               | Block [Statement]
               | Definition Pattern Expression
               | Assign Pattern Expression
               | Exec Expression
               | Print Expression
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

data Expression = Null
                | BoolVal Bool
                | Set (S.Set Expression)
                | IntNum Integer
                | FracNum Rational
                | Ref String
                | Func Pattern Expression
                | Applic Expression [Expression]
                | IfElse Expression Expression Expression -- if condition then expr else expr
                | Prefix PreOp Expression
                | Infix BinOp Expression Expression
                | Proc Statement
    deriving (Show, Eq, Ord)

data PreOp = Empty | Even | Odd | Neg
           | Minus | Plus
    deriving (Show, Enum, Eq, Ord)

data BinOp = Add | Sub | Mul | Div
           | Equals | NEquals | Less
    deriving (Show, Enum, Eq, Ord)