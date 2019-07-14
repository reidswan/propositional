module Propositional.Parser.Normal (parseProposition, parsePropositions) where

import Propositional.AST
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Combinator (eof)

{-
 | Syntax:
 |
 | digit ::= 0-9
 | alpha ::= a-zA-Z_
 | atom ::= alpha (alpha | digit)*
 | negAtom ::= ~ atom
 | and ::= (expr & expr)
 | or ::= (expr | expr)
 | expr ::= atom | negAtom | and | or
 -} 

type BinOp = NormalProposition -> NormalProposition -> NormalProposition

parseAtomId :: Parser String
parseAtomId = do 
    first <- letter
    rest <- many alphaNum
    return $ (first:rest)

parseAtom :: Parser NormalProposition
parseAtom = do
    s <- parseAtomId
    return $ NormAtom s

parseNegAtom :: Parser NormalProposition
parseNegAtom = do
    char '~'
    whitespaces
    a <- parseAtomId
    return $ NormNegAtom a

whitespaces :: Parser String
whitespaces = many $ oneOf " \t"

parseBinOp :: Parser BinOp
parseBinOp = do
    op <- oneOf "&|"
    return $ case op of 
        '&' -> NormAnd
        '|' -> NormOr

parseExpr = parseAtom <|> parseNegAtom <|> (do
    char '('
    whitespaces
    expr1 <- parseExpr
    whitespaces
    binOp <- parseBinOp
    whitespaces
    expr2 <- parseExpr
    whitespaces
    char ')'
    return $ binOp expr1 expr2)


parseTopLevelExpr :: Parser NormalProposition
parseTopLevelExpr = do
    expr <- parseExpr
    (char '\n' >> return ())  <|> eof
    return expr

parseMultipleTLP :: Parser [NormalProposition]
parseMultipleTLP = many1 (spaces >> parseTopLevelExpr)

parseProposition :: String -> Either String NormalProposition
parseProposition input = case parse parseTopLevelExpr "single propositional" input of
    Left err -> Left $ show err
    Right match -> Right match

parsePropositions :: String -> Either String [NormalProposition]
parsePropositions input = case parse parseMultipleTLP "propositional" input of
    Left err -> Left $ show err
    Right match -> Right match

instance ReadableProp NormalProposition where
    readProposition = parseProposition
    readPropositions = parsePropositions
    