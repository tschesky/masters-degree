-- This module defines a simple command line interface for the Boa
-- interpreter.  If your solution is correct, this module should just
-- work.
module Main (main) where

import BoaAST
import BoaInterp (execute)
  
import System.Environment(getArgs)

run :: Program -> IO ()
run p =
  do let (out, res) = execute p
     mapM putStrLn out
     case res of
       Nothing -> return ()
       Just e -> putStrLn ("*** Runtime error: " ++ show e)

main :: IO ()
main = do args <- getArgs
          case args of
            ["-i", file] -> do
              s <- readFile file
              run $ read s
            _ ->
              error "Usage: boa -i PROGRAM.ast"
