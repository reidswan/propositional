module Propositional.IO.Reader (
    readKnowledgeBase,
    hReadKnowledgeBase,
    withKnowledgeBase,
    hWithKnowledgeBase,
    withKnowledgeBase',
    hWithKnowledgeBase') where

import Propositional.AST
import System.IO

hWithKnowledgeBase :: (ReadableProp a) => Handle -> ([a] -> b) -> IO (Either String b)
hWithKnowledgeBase handle f = do
    src <- hGetContents handle
    case readPropositions src of
        Left err -> return $ Left err
        Right result -> return $ Right $ f result

withKnowledgeBase :: (ReadableProp a) => FilePath -> ([a] -> b) -> IO (Either String b)
withKnowledgeBase path f = withFile path ReadMode (flip hWithKnowledgeBase f)

hWithKnowledgeBase' :: (ReadableProp a) => Handle -> ([a] -> IO b) -> IO (Either String b)
hWithKnowledgeBase' handle f = do 
    src <- hGetContents handle
    case readPropositions src of 
        Left err -> return $ Left err
        Right kb -> do
            result <- f kb
            return $ Right result

withKnowledgeBase' :: (ReadableProp a) => FilePath -> ([a] -> IO b) -> IO (Either String b)
withKnowledgeBase' path f = withFile path ReadMode (flip hWithKnowledgeBase' f)

hReadKnowledgeBase :: (ReadableProp a) => Handle -> IO (Either String [a])
hReadKnowledgeBase handle = do
    src <- hGetContents handle
    return $ readPropositions src

readKnowledgeBase :: (ReadableProp a) => FilePath -> IO (Either String [a])
readKnowledgeBase path = withFile path ReadMode hReadKnowledgeBase
