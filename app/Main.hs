module Main where

import Propositional.Engine
import Propositional.AST
import Propositional.Parser
import Propositional.Parser.Normal
import Propositional.IO.Reader
import Propositional.IO.Writer
import Control.Monad (forM_)
import System.Environment (getArgs)

startsWith _ [] = True
startsWith [] _ = False
startsWith (s:src) (p:prefix) = s == p && src `startsWith` prefix

endsWith src postfix = reverse src `startsWith` reverse postfix

getKBFilePath :: [String] -> IO String
getKBFilePath args = if not $ null args then return $ head args else do
    putStr "Knowledge base filename: "
    getLine

runQuery :: Propositional a => [String] -> [a] -> IO ()
runQuery args kb = if length args >= 2
    then do
        let theQuerySrc = args !! 1
            theQueryRes = readProposition theQuerySrc :: Either String Proposition
        case theQueryRes of 
            Left err -> error $ "Failed to read query: " ++ err
            Right theQuery -> do
                let result = query kb theQuery
                putStrLn $ "KB |= " ++ show theQuery ++ " ? " ++ show result
    else queryLoop kb

queryLoop :: Propositional a => [a] -> IO ()
queryLoop kb = do
    putStr "Query: "
    querySrc <- getLine
    let theQueryRes = readProposition querySrc :: Either String Proposition
    case theQueryRes of 
        Left err -> putStrLn $ "Failed to read query: " ++ err
        Right theQuery -> do
            let result = query kb theQuery
            putStrLn $ "KB |= " ++ show theQuery ++ " ? " ++ show result
            queryLoop kb

main :: IO ()
main = do
    args <- getArgs
    kbFile <- getKBFilePath args
    result <- withKnowledgeBase' kbFile (runQuery args :: [Proposition] -> IO ())
    case result of 
        Left err -> error $ "An error occurred when parsing the knowledge base file:\n" ++ err
        _ -> return ()