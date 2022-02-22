module AST where

import qualified Data.Set as S

data Statement = Nop
               | Block [Statement]
               | Definition Pattern Expression
               | Assign Pattern Expression
               | Exec Expression
               | Print Expression
    deriving (Show, Eq, Ord)

isDef :: Statement -> Bool
isDef (Definition _ _) = True
isDef _                = False

data Pattern = Pattern String [(String, Type)]
    deriving (Show, Eq, Ord)

data Type = Top | Bottom
    deriving (Show, Eq, Ord)

data Expression = Null
                | BoolVal Bool
                | Set (S.Set Expression)
                | IntNum Integer
                | FracNum Rational
                | Ref String
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