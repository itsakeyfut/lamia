module Main where

import Lamia (processQuery)
import Lamia.Lexer
import Lamia.Parser
import Lamia.Evaluator

main :: IO ()
main = do
    putStrLn "Lamia SQL-like Query Language Interpreter"
    putStrLn "Enter your query (use :q to quit):"
    repl

repl :: IO ()
repl = do
    putStr "> "
    input <- getLine
    case input of
        ":q" -> putStrLn "Goodbye!"
        _    -> do
            let result = processQuery input
            putStrLn result
            repl
