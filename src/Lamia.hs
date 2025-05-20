module Lamia
  ( module Lamia.Lexer
  , module Lamia.Parser
  , module Lamia.AST
  , module Lamia.Evaluator
  , processQuery
  ) where

import Lamia.Lexer
import Lamia.Parser
import Lamia.AST
import Lamia.Evaluator

-- | クエリを処理する
processQuery :: String -> String
processQuery input = 
  case runLexer input of
    Left err -> "Lexer error: " ++ show err
    Right tokens -> 
      case runParser tokens of
        Left err -> "Parser error: " ++ show err
        Right ast -> evaluateAst ast