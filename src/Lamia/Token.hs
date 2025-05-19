module Lamia.Lexer.Token
    ( TokenType(..)
    , Token(..)
    ) where

-- | TokenType
data TokenType
    -- Keywords
    = SELECT | FROM | WHERE | INSERT | UPDATE | DELETE | CREATE | ALTER | DROP | TABLE
    | INTO | VALUES | SET | AND | OR | NOT | NULL | IS | IN | LIKE | BETWEEN | AS | JOIN
    | LEFT | RIGHT | INNER | OUTER | ON | GROUP | ORDER | BY | HAVING | LIMIT | OFFSET
    | ASC | DESC | DISTINCT | UNION | ALL | CASE | WHEN | THEN | EKSE | END | IF | EXISTS

    -- ADT
    | INT | VARCHAR | TEXT | DATE | DATETIME | FLOAT | DOUBLE | BOOLEAN | CHAR

    -- Literals
    | STRING_LITERAL | NUMERIC_LITERAL

    -- Identifiers
    | IDENTIFIER

    -- Operators
    | EQUAL | NOT_EQUAL | GREATER | LESS | GREATER_EQUAL | LESS_EQUAL | PLUS | MINUS
    | MULTIPLY | DIVIDE | MOD

    -- Punctuation
    | SEMICOLON | COMMA | DOT | LEFT_PAREN | RIGHT_PAREN | ASTERISK

    -- Miscellaneous
    | EOF | COMMENT | UNKNOWN
    deriving (Show, Eq)

-- | Token
data Token = Token
    { tokenType  :: TokenType -- ^ The type of the token
    , tokenValue :: String    -- ^ The value of the token
    , tokenLine  :: Int       -- ^ The line number where the token was found
    , tokenPos   :: Int       -- ^ The position of the token in the line
    } deriving (Show, Eq)
