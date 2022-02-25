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

    P.identStart = letter <|> oneOf "_",
    P.identLetter = alphaNum <|> oneOf "_",
    P.opStart = oneOf "!@#$%^&*()-=+{}[]/.,<>;:∅",
    P.opLetter = oneOf "!@#$%^&*()-=+{}[]/.,<>;:∅",

    P.reservedNames = ["print", "NOP", "true", "false", "proc", "func", "∅"],
    P.reservedOpNames = [":=", "<-", "{", "}", ","],

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
                    E.Infix (reservedOp "*" $> Infix Mul) E.AssocLeft,
                    E.Infix (reservedOp "/" $> Infix Div) E.AssocLeft
                ],[
                    E.Infix (reservedOp "+" $> Infix Add) E.AssocLeft,
                    E.Infix (reservedOp "-" $> Infix Sub) E.AssocLeft
                ],[
                    E.Infix (reservedOp "=" $> Infix Equals) E.AssocNone,
                    E.Infix (reservedOp "/=" $> Infix NEquals) E.AssocNone,
                    E.Infix (reservedOp "<" $> Infix Less) E.AssocNone
                ]
           ]


runParser :: Parser a -> String -> a
runParser p str = case runIndentParser p () "" str of
    Left err -> error $ "parse error at " ++ show err
    Right val -> val

parse :: String -> Statement
parse = Parser.runParser (P.whiteSpace lexer >> topLevel >> (block statementParser <&> Block))

setLiteralParser :: Parser Expression
setLiteralParser = (reserved "∅" $> Null) <|> do
    es <- braces $ sepBy expressionParser (reservedOp ",")
    return $
        if null es then Null else Set (S.fromList es)

printParser :: Parser Statement
printParser = reserved "print" >> expressionParser <&> Print

expressionParser :: Parser Expression
expressionParser = E.buildExpressionParser opsTable termsParser

termsParser :: Parser Expression
termsParser = do
                  t <- termParser
                  ts <- many (try $ indented >> (termParser <* notFollowedBy (reservedOp "<-")))
                  return $ case ts of
                      [] -> t
                      ts -> Applic t ts
         
termParser :: Parser Expression
termParser =    (IntNum <$> natural)
            <|> (reserved "true" $> BoolVal True)
            <|> (reserved "false" $> BoolVal False)
            <|> (Ref <$> identifier)
            <|> setLiteralParser
            <|> parens expressionParser

statementParser :: Parser Statement
statementParser =
    try (Exec <$> (expressionParser <* notFollowedBy (reservedOp "<-")))
    <|> assignParser
    <|> definitionParser
    <|> printParser
    <|> (reserved "NOP" $> Nop)

blockParser :: Parser Statement
blockParser = block statementParser <&> Block -- TODO if only one statement dont wrap it in a block

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