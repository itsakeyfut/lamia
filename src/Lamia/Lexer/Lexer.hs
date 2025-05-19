module Lamia.Lexer.Lexer
    ( runLexer
    , lexer
    , LexerError
    ) where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Map qualified as Map
import Text.Parsec
import Text.Parsec.String (Parser)

import Lamia.Lexer.Token

type LexerError = ParseError

-- | Mapping of keywords to their corresponding token types
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
    -- ADT
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

-- | Lexer
runLexer :: String -> Either LexerError [Token]
runLexer input = parse lexer "" input

-- | Lexer parser
lexer :: Parser [Token]
lexer = do
    tokens <- many token
    eof <- eofToken
    return $ tokens ++ [eof]

-- | Token parser
token :: Parser Token
token = do
    -- Skip whitespace
    skipMany (void space <|> void lineComment <|> void blockComment)

    -- Fetch the position and line number
    pos <- getPosition
    let line = sourceLine pos
    let column = sourceColumn pos

    -- Parse the token
    tokenParser line column

-- | Token parser for different token types
tokenParser :: Int -> Int -> Parser Token
tokenParser line column =
        keywordOrIdentifier line column
    <|> numericLiteral line column
    <|> stringLiteral line column
    <|> operatorOrDelimiter line column

-- | Keyword or identifier parser
keywordOrIdentifier :: Int -> Int -> Parser Token
keywordOrIdentifier line column = do
    first <- letter <|> char '_'
    rest <- many (aplhaNum <|> char '_')
    let word = first : rest
    let upperWord = map tpUpper word

    return $ case Map.lookup upperWord keywords of
        Just tokenType -> Token tokenType word line column
        Nothing -> Token IDENTIFIER word line column
    where
        tpUpper c = if 'a' <= c && c <= 'z' then toEnum (fromEnum c - 32) else c

-- | Numeric literal parser
numericLiteral :: Int -> Int -> Parser Token
numericLiteral line column = do
    digit1 <- many1 digit

    -- Optional decimal point and fractional part
    decimal <- option "" $ do
        char '.'
        difit2 <- many1 digit
        return $ '.' : digit2

    let value = digit1 ++ decimal
    return $ Token NUMERIC_LITERAL value line column

-- | String literal parser
stringLiteral :: Int -> Int -> Parser Token
stringLiteral line column = do
    quote <- char '\'' <|> char '\"'
    value <- many (escapedChar quote <|> noneOf [quote])
    _ <- char quote
    return $ Token STRING_LITERAL value line column
    where
        escapedChar quote = do
            _ <- char '\\'
            c <- oneOf ['\\', '\"', '\'', 'n', 'r', 't']
            return $ case c of
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'
                _   -> c

-- | Operator or delimiter parser
operatorOrDelimiter :: Int -> Int -> Parser Token
operatorOrDelimiter line column = do
    pos <- getPosition
    c <- anyChar

    let currentColumn = sourceColumn pos
    let tokenInfo = case c of
            -- Operators
            '=' -> (EQUAL, "=")
            '>' -> try (do
                _ <- char '='
                return (GREATER_EQUAL, ">=")
                ) <|> return (GREATER, ">")
            '<' -> try (do
                _ <- char '='
                return (LESS_EQUAL, "<=")
                ) <|> try (do
                _ <- char '>'
                return (NOT_EQUAL, "<>")
                ) <|> return (LESS, "<")
            '!' -> try (do
                _ <- char '='
                return (NOT_EQUAL, "!=")
                )
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
            
            -- Miscellaneous
            _   -> (UNKNOWN, [c])

    return $ Token (fst tokenInfo) (snd tokenInfo) line column

-- | Comment parser
lineComment :: Parser ()
lineComment = do
    _ <- try (string "--")
    _ <- manyTill anyChar (try endOfLine <|> try (eof >> return '\n'))
    return ()

-- | Block comment parser
blockComment :: Parser ()
blockComment = do
    _ <- try (string "/*")
    _ <- manyTill anyChar (try (string "*/"))
    return ()

-- | End of file token
eofToken :: Parser Token
eofToken = do
    pos <- getPosition
    return $ Toekn EOF "" (sourceLine pos) (sourceColumn pos)
