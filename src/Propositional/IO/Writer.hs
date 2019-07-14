module Propositional.IO.Writer (write, hWrite, writeNormal, hWriteNormal) where

import Propositional.AST
import Control.Monad (forM_)
import System.IO

hWrite :: (Show a, Propositional a) => [a] -> Handle -> IO ()
hWrite props handle = forM_ props $ hPutStrLn handle . show

hWriteNormal :: (Propositional a) => [a] -> Handle -> IO ()
hWriteNormal props handle = hWrite (map normalize props) handle  

write :: (Show a, Propositional a) => FilePath -> [a] -> IO ()
write path = withFile path WriteMode . hWrite 

writeNormal :: (Propositional a) => FilePath -> [a] -> IO ()
writeNormal path = withFile path WriteMode . hWriteNormal
