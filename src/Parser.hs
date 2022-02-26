{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import AST

import Text.Parsec (ParsecT, try)
import Text.ParserCombinators.Parsec hiding (Parser, parsem, try)
import Text.Parsec.Indent
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Expr as E
import qualified Text.Parsec.Language as L
import Data.Functor.Identity
import Data.Functor
import qualified Data.Set as S


type Parser a = ParsecT String () (IndentT Identity) a

lexer :: P.GenTokenParser String () (IndentT Identity)
lexer = P.makeTokenParser $ P.LanguageDef {
    P.commentStart = "-/",
    P.commentEnd   = "/-",
    P.commentLine  = "--" ,
    P.nestedComments = True,

    P.identStart = letter <|> oneOf "_",
    P.identLetter = alphaNum <|> oneOf "_",
    P.opStart = oneOf "!@#$%^&*()-=+{}[]/.,<>;:∅",
    P.opLetter = oneOf "!@#$%^&*()-=+{}[]/.,<>;:∅",

    P.reservedNames = ["NOP", "ECHO", "∅",
                      "true", "false",
                      "proc", "func",
                      "and", "or",
                      
                      "if", "then", "else"],
    P.reservedOpNames = [":=", "<-", "{", "}", ",",
                        "≠", "≥", "≤"],

    P.caseSensitive = True
}

symbol = P.symbol lexer
integer = P.integer lexer
natural = P.natural lexer
identifier = P.identifier lexer
stringLiteral = P.stringLiteral lexer
parens = P.parens lexer
braces = P.braces lexer
reserved = P.reserved lexer
reservedOp = P.reservedOp lexer


opsTable :: [[E.Operator String () (IndentT Identity) Expression]]
opsTable = [
                [
                    E.Prefix (reservedOp "+" $> Prefix Plus), 
                    E.Prefix (reservedOp "-" $> Prefix Minus)
                ],[
                    E.Prefix (reserved "empty" $> Prefix Empty),
                    E.Prefix (reserved "even" $> Prefix Even),
                    E.Prefix (reserved "odd" $> Prefix Odd),
                    E.Prefix (reserved "neg" $> Prefix Neg)
                ],[
                    E.Infix (reservedOp "*" $> Infix Mul) E.AssocLeft,
                    E.Infix (reservedOp "/" $> Infix Div) E.AssocLeft
                ],[
                    E.Infix (reservedOp "+" $> Infix Add) E.AssocLeft,
                    E.Infix (reservedOp "-" $> Infix Sub) E.AssocLeft
                ],[
                    E.Infix (reservedOp "=" $> Infix Equals) E.AssocNone,
                    E.Infix ((reservedOp "/=" <|> reservedOp "≠")$> Infix NEquals) E.AssocNone,
                    E.Infix (reservedOp "<" $> Infix Less) E.AssocNone,
                    E.Infix (reservedOp ">" $> Infix Greater) E.AssocNone,
                    E.Infix ((reservedOp "<=" <|> reservedOp "≤") $> Infix LessEq) E.AssocNone,
                    E.Infix ((reservedOp ">=" <|> reservedOp "≥") $> Infix GreaterEq) E.AssocNone
                ],[
                    E.Infix (reserved "and" $> Infix BAnd) E.AssocLeft
                ],[
                    E.Infix (reserved "or" $> Infix BOr) E.AssocLeft
                ]
           ]


runParser :: Parser a -> String -> a
runParser p str = case runIndentParser p () "" str of
    Left err -> error $ "parse error at " ++ show err
    Right val -> val

parse :: String -> Statement
parse = Parser.runParser (P.whiteSpace lexer >> blockParser)

setLiteralParser :: Parser Expression
setLiteralParser = (reserved "∅" $> Null) <|> do
    es <- braces $ sepBy expressionParser (reservedOp ",")
    return $
        if null es then Null else Set (S.fromList es)

expressionParser :: Parser Expression
expressionParser = E.buildExpressionParser opsTable
    (termsParser <&> \(t:ts) -> case ts of
                    [] -> t
                    _ -> Applic t ts)

termsParser :: Parser [Expression]
termsParser = try $ do
    t <- withPos termParser
    ts <- many $ sameOrIndented >> termParser
    return (t:ts)
         
termParser :: Parser Expression
termParser =    try (IntNum <$> natural)
            <|> try (reserved "true" $> BoolVal True)
            <|> try (reserved "false" $> BoolVal False)
            <|> try (Ref <$> identifier)
            <|> try (StrVal <$> stringLiteral)
            <|> try setLiteralParser
            <|> try ifThenElseParser
            <|>     parens expressionParser

statementParser :: Parser Statement
statementParser =
        try (Exec <$> (expressionParser <* notFollowedBy (reservedOp "<-")))
    <|> try ifStatementParser
    <|> try assignParser
    <|> try definitionParser
    <|> try (reserved "NOP" $> Nop)
    <|> try (reserved "ECHO" $> Echo)

blockParser :: Parser Statement
blockParser = withPos $ block statementParser <&> Block

ifThenElseParser :: Parser Expression
ifThenElseParser = try $ do
    reserved "if"
    cond <- expressionParser
    reserved "then"
    trueBranch <- expressionParser
    reserved "else"
    falseBranch <- expressionParser
    return $ IfThenElse cond trueBranch falseBranch

ifStatementParser :: Parser Statement
ifStatementParser = try $ do
    withPos $ reserved "if"
    cond <- expressionParser
    reservedOp ":"
    trueBranch <- indented >> blockParser
    falseBranch <- optionMaybe $ do
        --checkIndent
        reserved "else"
        (do
            reservedOp ":"
            indented >> blockParser) <|> ifStatementParser
    return $ IfStatement cond trueBranch falseBranch

patternParser :: Parser Pattern
patternParser = try $ do
    (name:params) <- many1 identifier
    return $ Pattern name $ zip params (repeat Top)

assignParser :: Parser Statement
assignParser = try (Assign <$> (patternParser <* reservedOp "<-") <*> expressionParser)

definitionParser :: Parser Statement
definitionParser =
        try (funcDef <$> (reserved "func" >> patternParser <* reservedOp ":=") <*> expressionParser)
    <|>     (procDef <$> (reserved "proc" >> patternParser <* reservedOp ":" ) <*> (indented >> blockParser))