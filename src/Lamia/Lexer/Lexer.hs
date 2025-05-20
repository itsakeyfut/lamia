module Lamia.Lexer.Lexer
  ( runLexer
  , lexer
  , LexerError
  ) where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import qualified Data.Map as Map
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)

import Lamia.Lexer.Token

type LexerError = P.ParseError

-- | Mapping of keywords to token types
keywords :: Map.Map String TokenType
keywords = Map.fromList
  [ ("SELECT", SELECT)
  , ("FROM", FROM)
  , ("WHERE", WHERE)
  , ("INSERT", INSERT)
  , ("UPDATE", UPDATE)
  , ("DELETE", DELETE)
  , ("CREATE", CREATE)
  , ("ALTER", ALTER)
  , ("DROP", DROP)
  , ("TABLE", TABLE)
  , ("INTO", INTO)
  , ("VALUES", VALUES)
  , ("SET", SET)
  , ("AND", AND)
  , ("OR", OR)
  , ("NOT", NOT)
  , ("NULL", NULL)
  , ("IS", IS)
  , ("IN", IN)
  , ("LIKE", LIKE)
  , ("BETWEEN", BETWEEN)
  , ("AS", AS)
  , ("JOIN", JOIN)
  , ("LEFT", LEFT)
  , ("RIGHT", RIGHT)
  , ("INNER", INNER)
  , ("OUTER", OUTER)
  , ("ON", ON)
  , ("GROUP", GROUP)
  , ("ORDER", ORDER)
  , ("BY", BY)
  , ("HAVING", HAVING)
  , ("LIMIT", LIMIT)
  , ("OFFSET", OFFSET)
  , ("ASC", ASC)
  , ("DESC", DESC)
  , ("DISTINCT", DISTINCT)
  , ("UNION", UNION)
  , ("ALL", ALL)
  , ("CASE", CASE)
  , ("WHEN", WHEN)
  , ("THEN", THEN)
  , ("ELSE", ELSE)
  , ("END", END)
  , ("IF", IF)
  , ("EXISTS", EXISTS)
  -- Data type keywords
  , ("INT", INT)
  , ("VARCHAR", VARCHAR)
  , ("TEXT", TEXT)
  , ("DATE", DATE)
  , ("DATETIME", DATETIME)
  , ("FLOAT", FLOAT)
  , ("DOUBLE", DOUBLE)
  , ("BOOLEAN", BOOLEAN)
  , ("CHAR", CHAR)
  ]

-- | Run lexer on input string
runLexer :: String -> Either LexerError [Token]
runLexer input = P.parse lexer "" input

-- | Main lexer - generates list of tokens
lexer :: Parser [Token]
lexer = do
  tokens <- P.many scanToken
  eof <- eofToken
  return $ tokens ++ [eof]

-- | Parse a single token
scanToken :: Parser Token
scanToken = do
  -- Skip whitespace and comments
  P.skipMany (void P.space P.<|> void lineComment P.<|> void blockComment)
  
  -- Get position info
  pos <- P.getPosition
  let line = P.sourceLine pos
  let column = P.sourceColumn pos
  
  -- Parse token
  tokenParser line column

-- | Token parser
tokenParser :: Int -> Int -> Parser Token
tokenParser line column =
      keywordOrIdentifier line column
  P.<|> numericLiteral line column
  P.<|> stringLiteral line column
  P.<|> operatorOrDelimiter line column

-- | Parse keyword or identifier
keywordOrIdentifier :: Int -> Int -> Parser Token
keywordOrIdentifier line column = do
  first <- P.letter P.<|> P.char '_'
  rest <- P.many (P.alphaNum P.<|> P.char '_')
  let word = first : rest
  let upperWord = map toUpper word
  
  return $ case Map.lookup upperWord keywords of
    Just tokenType -> Token tokenType word line column
    Nothing        -> Token IDENTIFIER word line column
  where
    toUpper c = if 'a' <= c && c <= 'z' then toEnum (fromEnum c - 32) else c

-- | Parse numeric literal
numericLiteral :: Int -> Int -> Parser Token
numericLiteral line column = do
  digits1 <- P.many1 P.digit
  
  -- Parse decimal part
  decimal <- P.option "" $ do
    _ <- P.char '.'
    digits2 <- P.many1 P.digit
    return $ '.' : digits2
  
  let value = digits1 ++ decimal
  return $ Token NUMERIC_LITERAL value line column

-- | Parse string literal
stringLiteral :: Int -> Int -> Parser Token
stringLiteral line column = do
  quote <- P.char '\'' P.<|> P.char '"'
  value <- P.many (parseEscapeSeq P.<|> P.noneOf [quote])
  _ <- P.char quote
  return $ Token STRING_LITERAL value line column

-- | Parse escape sequence
parseEscapeSeq :: Parser Char
parseEscapeSeq = do
  _ <- P.char '\\'
  c <- P.oneOf ['\\', '"', '\'', 'n', 'r', 't']
  return $ case c of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    _   -> c

-- | Parse operators or delimiters
operatorOrDelimiter :: Int -> Int -> Parser Token
operatorOrDelimiter line column = do
  pos <- P.getPosition
  c <- P.anyChar
  
  let currentColumn = P.sourceColumn pos
  let tokenInfo = case c of
        -- Operators
        '=' -> (EQUAL, "=")
        '>' -> P.try (do
                  _ <- P.char '='
                  return (GREATER_EQUAL, ">=")
                ) P.<|> return (GREATER, ">")
        '<' -> P.try (do
                  _ <- P.char '='
                  return (LESS_EQUAL, "<=")
                ) P.<|> P.try (do
                  _ <- P.char '>'
                  return (NOT_EQUAL, "<>")
                ) P.<|> return (LESS, "<")
        '!' -> P.try (do
                  _ <- P.char '='
                  return (NOT_EQUAL, "!=")
                ) P.<|> return (UNKNOWN, "!")
        '+' -> (PLUS, "+")
        '-' -> (MINUS, "-")
        '*' -> (ASTERISK, "*")
        '/' -> (DIVIDE, "/")
        '%' -> (MOD, "%")
        
        -- Delimiters
        ';' -> (SEMICOLON, ";")
        ',' -> (COMMA, ",")
        '.' -> (DOT, ".")
        '(' -> (LEFT_PAREN, "(")
        ')' -> (RIGHT_PAREN, ")")
        
        -- Other
        _   -> (UNKNOWN, [c])
  
  return $ Token (fst tokenInfo) (snd tokenInfo) line column

-- | Parse line comment (-- to end of line)
lineComment :: Parser ()
lineComment = do
  _ <- P.try (P.string "--")
  _ <- P.manyTill P.anyChar (P.try P.endOfLine P.<|> P.try (P.eof >> return '\n'))
  return ()

-- | Parse block comment (/* ... */)
blockComment :: Parser ()
blockComment = do
  _ <- P.try (P.string "/*")
  _ <- P.manyTill P.anyChar (P.try (P.string "*/"))
  return ()

-- | Create EOF token
eofToken :: Parser Token
eofToken = do
  pos <- P.getPosition
  return $ Token EOF "" (P.sourceLine pos) (P.sourceColumn pos)