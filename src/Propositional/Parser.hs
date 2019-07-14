module Propositional.Parser (parseProposition, parsePropositions) where

import Propositional.AST
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator (eof)

{-
 | Syntax:
 |
 | digit ::= 0-9
 | alpha ::= a-zA-Z_
 | atom ::= alpha (alpha | digit)*
 | matImpExpr = orExpr -> expr
 | orExpr = andExpr `| expr
 | andExpr = unExpr (&|^) expr
 | unExpr = atom | ~ unExpr | `(expr`)
 | expr = matImpExpr | orExpr | andExpr | unExpr
 -} 

parseAtom :: Parser Proposition
parseAtom = do 
    first <- letter
    rest <- many alphaNum
    return $ Atom (first:rest)

whitespaces :: Parser String
whitespaces = many $ oneOf " \t"

parseMatImpExpr :: Parser Proposition
parseMatImpExpr = try (do
    exp1 <- parseOrExpr
    whitespaces
    string "->"
    whitespaces
    exp2 <- parseMatImpExpr 
    return $ Implies exp1 exp2) <|> parseOrExpr

parseOrExpr :: Parser Proposition
parseOrExpr = try (do
    exp1 <- parseAndExpr
    whitespaces
    char '|'
    whitespaces
    exp2 <- parseOrExpr
    return $ Or exp1 exp2) <|> parseAndExpr

parseAndExpr :: Parser Proposition
parseAndExpr = try (do
    exp1 <- parseUnExpr
    whitespaces
    oneOf "^&"
    whitespaces
    exp2 <- parseAndExpr
    return $ And exp1 exp2) <|> parseUnExpr

parseUnExpr :: Parser Proposition
parseUnExpr = parseAtom <|> (do 
    char '~'
    whitespaces 
    expr <- parseUnExpr
    return $ Neg expr) <|> (do
    char '('
    expr <- parseExpr
    char ')'
    return $ expr)

parseExpr :: Parser Proposition
parseExpr = do
    whitespaces
    expr <- parseMatImpExpr
    whitespaces
    return expr

parseTopLevelExpr :: Parser Proposition
parseTopLevelExpr = do
    expr <- parseExpr
    (char '\n' >> return ())  <|> eof
    return expr

parseMultipleTLP :: Parser [Proposition]
parseMultipleTLP = many1 (spaces >> parseTopLevelExpr)

parseProposition :: String -> Either String Proposition
parseProposition input = case parse parseTopLevelExpr "single propositional" input of
    Left err -> Left $ show err
    Right match -> Right match

parsePropositions :: String -> Either String [Proposition]
parsePropositions input = case parse parseMultipleTLP "propositional" input of
    Left err -> Left $ show err
    Right match -> Right match

instance ReadableProp Proposition where
    readProposition = parseProposition
    readPropositions = parsePropositions
