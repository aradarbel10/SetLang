{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Parser(Parser.parse) where

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

    P.identStart = letter,
    P.identLetter = alphaNum <|> char '_',
    P.opStart = oneOf "!@#$%^&*()-=+{}[]/.,<>;:",
    P.opLetter = oneOf "!@#$%^&*()-=+{}[]/.,<>;:",

    P.reservedNames = ["print", "NOP", "true", "false", "do", "def"],
    P.reservedOpNames = [],

    P.caseSensitive = True
}

symbol = P.symbol lexer
integer = P.integer lexer
natural = P.natural lexer
identifier = P.identifier lexer
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
                    E.Infix (reservedOp "=" $> Infix Equals) E.AssocNone,
                    E.Infix (reservedOp "/=" $> Infix NEquals) E.AssocNone,
                    E.Infix (reservedOp "<" $> Infix Less) E.AssocNone
                ],[
                    E.Infix (reservedOp "*" $> Infix Mul) E.AssocLeft,
                    E.Infix (reservedOp "/" $> Infix Div) E.AssocLeft
                ],[
                    E.Infix (reservedOp "+" $> Infix Add) E.AssocLeft,
                    E.Infix (reservedOp "-" $> Infix Sub) E.AssocLeft
                ]
           ]


runParser :: Parser a -> String -> a
runParser p str = case runIndentParser p () "" str of
    Left err -> error $ "parse error at " ++ show err
    Right val -> val

parse :: String -> Statement
parse = Parser.runParser (P.whiteSpace lexer >> topLevel >> (block statementParser <&> Block))

setLiteralParser :: Parser Expression
setLiteralParser = (reservedOp "∅" $> Null) <|> do
    es <- braces $ sepBy expressionParser (symbol ",")
    return $
        if null es then Null else Set (S.fromList es)

printParser :: Parser Statement
printParser = reserved "print" >> expressionParser <&> Print

expressionParser :: Parser Expression
expressionParser = E.buildExpressionParser opsTable termParser

termParser :: Parser Expression
termParser = foldl1 Applic <$> many1 (sameOrIndented >> try ((
                (IntNum <$> natural)
            <|> (reserved "true" $> BoolVal True)
            <|> (reserved "false" $> BoolVal False)
            <|> (Ref . Name <$> identifier)
            <|> setLiteralParser
            <|> procParser
            <|> parens expressionParser)
                <* notFollowedBy (
                        reserved "<-"
                    <|> reserved ":="
                )))
         

statementParser :: Parser Statement
statementParser =
        try definitionParser
    <|> try printParser
    <|>     (reserved "NOP" $> Nop)

blockParser :: Parser Statement
blockParser = indented >> (block statementParser <&> Block)

patternParser :: Parser Pattern
patternParser = do
    name <- identifier
    ps <- optionMaybe (parens $ sepBy1 identifier (symbol ","))
    return $ case ps of
        Nothing -> Name name
        Just plist -> Func name plist

procParser :: Parser Expression
procParser = reserved "do" >> Proc <$> blockParser

definitionParser :: Parser Statement
definitionParser =
        (Assign     <$> (                  patternParser <* symbol "<-") <*> expressionParser)
    <|> (Definition <$> (reserved "def" >> patternParser <* symbol ":=") <*> expressionParser)