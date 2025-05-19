module Lamia.Parser.Parser
    ( parseSQL
    , runParser
    , ParseError
    ) where

import Control.Monad (void)
import Data.Functor (($>))
import Text.Parsec
import Text.Parsec.Pos (newPos)
import qualified Text.Parsec.Pos as Pos

import Lamia.AST
import Lamia.Lexer.Token

type TokenParser = Parsec [Token] ()
type ParserError = ParseError

-- | Parse a SQL statement from a list of tokens
parseSQL :: [Token] -> Either ParserError SQLStatement
parseSQL = runParser sqlStmt () "<input>"

-- | Run the parser on the input tokens
runParser :: [Token] -> Either ParserError SQLStatement
runParser parserSQL

-- | Parse a SQL statement
sqlStmt :: TokenParser SQLStatement
sqlStmt = selectStmt <* eof
    where
        selectStmt = SelectStmt selectStatement

-- | Check if the token is of a specific type
satisfyToken :: (TokenType -> Bool) -> (String -> Maybe a) -> TokenParser as
satisfyToken typePred extract = token showToken posFromToken testToken
    where
        showToken t = show (tokenType t) ++ " " ++ tokenValue t
        posFromToken t = newPos "<input>" (tokenLine t) (tokenColumn t)
        testToken t = if typePred (tokenType t)
                        then extract (tokenValue t)
                        else Nothing

-- | Specific token type
tokenOfType :: TokenType -> TokenParser String
tokenOfType expectedType = satisfyToken (== expectedType) Just

-- | Keyword token
keyword :: TokenType -> TokenParser ()
keyword expectedType = void $ tokenOfType expectedType

-- | Identifier token
identifier :: TokenParser String
identifier = satisfyToken (== IDENTIFIER) Just

-- | String literal token
stringLiteral :: TokenParser String
stringLiteral = satisfyToken (== STRING_LITERAL) Just

-- | Numeric literal token
numericLiteral :: TokenParser String
numericLiteral = satisfyToken (== NUMERIC_LITERAL) Just

-- | SELECT statement
selectStatement :: TokenParser selectStatement
selectStatement = do
    -- SELECT [DISTINCT]
    keyword SELECT
    distinct <- option False (keyword DISTINCT $> True)

    -- select_list
    columns <- select_list
    
    -- FROM caluse 
    fromClause <- optionMaybe $ do
        keyword FROM
        tableExpr

    -- WHERE clause
    whereClause <- optionMaybe $ do
        keyword WHERE
        expression

    -- GROUP BY clause
    groupByClause <- optionMaybe $ do
        keyword GROUP
        keyword BY
        sepBy1 expression (keyword COMMA)

    -- HAVING clause
    havingClause <- optionMaybe $ do
        keyword HAVING
        expression

    -- ORDER BY clause
    orderByClause <- optionMaybe $ do
        keyword ORDER
        keyword BY
        sepBy1 orderByExpression (keyword COMMA)

    -- LIMIT clause
    limitClause <- optionMaybe $ do
        keyword LIMIT
        read <$> numericLiteral

    -- OFFSET clause
    offsetClause <- optionMaybe $ do
        keyword OFFSET
        read <$> numericLiteral

    return selectStatement
        { selectDistinct = distinct
        , selectList = columns
        , selectFrom = fromClause
        , selectWhere = whereClause
        , selectGroupBy = groupByClause
        , selectHaving = havingClause
        , selectOrderBy = orderByClause
        , selectLimit = limitClause
        , selectOffset = offsetClause
        }

-- | Column list parser
selectList :: TokenParser [(Expression, Maybe String)]
selectList = sepBy1 selectItem (keyword COMMA)

-- | Select item parser
selectItem :: TokenParser (Expression, Maybe String)
selectItem = do
    expr <- expression

    -- AS alias
    alias <- optionMaybe $ do
        option () (keyword AS)
        identifier

    return (expr, alias)

-- | Table expression parser
tableExpr :: TokenParser TableExpr
tableExpr =
        tableJoin
    <|> tableSubQuery
    <|> tableName

-- | Table reference parser
tableName :: TokenParser TableExpr
tableName = do
    name <- identifier

    -- AS alias
    alias <- optionMaybe $ do
        option () (keyword AS)
        identifier

    return $ TableName name alias

-- | Subquery parser
tableSubQuery :: TokenParser TableExpr
tableSubQuery = do
    keyword LEFT_PAREN
    query <- selectStatement
    keyword RIGHT_PAREN

    -- AS alias
    keyword AS
    alias <- identifier

    return $ tableSubQuery query alias

-- | Join parser
tableJoin :: TokenParser TableExpr
tableJoin = do
    left <- tableName <|> tableSubQuery

    joinType <- joinTypeParser
    right <- tablename <|> tableSubQuery

    -- ON condition
    keyword ON
    onExpr <- expression

    return $ TableJoin $ JoinClause
        { joinLeft = left
        , joinType = joinType
        , joinRight = right
        , joinOn = onExpr
        }

-- | Join type parser
joinTypeParser :: TokenParser JoinType
joinTypeParser =
        (keyword INNER >> keyword JOIN $> InnerJoin)
    <|> (keyword LEFT >> option () (keyword OUTER) >> keyword JOIN $> LeftJoin)
    <|> (keyword RIGHT >> option () (keyword OUTER) >> keyword JOIN $> RightJoin)
    <|> (keyword CROSS >> keyword JOIN $> CrossJoin)
    <|> (keyword JOIN $> InnerJoin) -- Default to INNER JOIN if no type is specified

-- | Expression parser
expression :: TokenParser Expression
expression = logicalOr

-- | Logical OR expression parser
logicalOr :: TokenParser Expression
logicalOr = chainl1 logicalAnd orOp
    where
        orOp = keyword OR $> (\lhs rhs -> BinaryExpr Or lhs rhs)

-- | Logical AND expression parser
logicalAnd :: TokenParser Expression
logicalAnd = chainl1 comparison andOp
    where
        andOp = keyword AND $> (\lhs rhs -> BinaryExpr And lhs rhs)

-- | Comparison expression parser
comparison :: TokenParser Expression
comparison = do
    expr <- additiveExpr

    option expr $ do
        op <- comparisonOp
        rhs <- additiveExpr
        return $ BinaryExpr or expr rhs

    where
        comparisonOp =
            (tokenOfType EQUALS $> Equal)
        <|> (tokenOfType NOT_EQUALS $> NotEqual)
        <|> (tokenOfType GREATER $> Greater)
        <|> (tokenOfType LESS $> Less)
        <|> (tokenOfType GREATER_EQUALS $> GreaterEqual)
        <|> (tokenOfType LESS_EQUALS $> LessEqual)

-- | Additive expression parser
additiveExpr :: TokenParser Expression
additiveExpr = chainl1 multiplicativeExpr addOp
    where
        addOp = 
            (tokenOfType PLUS $> (\lhs rhs -> BinaryExpr Plus lhs rhs))
        <|> (tokenOfType MINUS $> (\lhs rhs -> BinaryExpr Minus lhs rhs))

-- | Multiplicative and Division expression parser
multiplicativeExpr :: TokenParser Expression
multiplicativeExpr = chainl1 primaryExpr mulOp
    where
        mulOp =
            (tokenOfType ASTERISK $> (\lhs rhs -> BinaryExpr Multiply lhs rhs))
        <|> (tokenOfType DIVIDE $> (\lhs rhs -> BinaryExpr Divide lhs rhs))

-- | Primary expression parser
primaryExpr :: TokenParser Expression
primaryExpr =
        literalExpr
    <|> columnRefExpr
    <|> functionCallExpr
    <|> caseExpr
    <|> existsExpr
    <|> subQueryExpr
    <|> parensExpr

-- | Parenthesized expression parser
parensExpr :: TokenParser Expression
parensExpr = do
    keyword LEFT_PAREN
    expr <- expression
    keyword RIGHT_PAREN
    return expr

-- | Literal expression parser
literalExpr :: TokenParser Expression
literalExpr =
        (LiteralExpr . StringLit <$> stringLiteral)
    <|> (LiteralExpr . NumericLit <$> numericLiteral)
    <|> (keyword NULL $> LiteralExpr NullLit)
    <|> (try $ keyword TRUE $> LiteralExpr (BoolLit True))
    <|> (try $ keyword FALSE $> LiteralExpr (BoolLit False))

-- | Column reference expression parser
columnRefExpr :: TokenParser Expression
columnRefExpr = do
    -- ident1.ident2 or ident
    first <- identifier

    tableName <- optionMaybe $ do
    keyword DOT
    identifier

    case tableName of
    Just table -> 
        -- first.table (table.column)
        return $ ColumnRef table (Just first)
    Nothing -> 
        -- just first (column)
        return $ ColumnRef first Nothing

-- | Function call expression parser
functionCallExpr :: TokenParser Expression
functionCallExpr = do
    funcName <- identifier
    keyword LEFT_PAREN

    -- Special case for COUNT(*)
    args <- if funcName == "COUNT" || funcName == "count"
            then countArgsParser
            else sepBy expression (keyword COMMA)

    keyword RIGHT_PAREN
    return $ FunctionCall funcName args
    where
    countArgsParser = do
        (tokenOfType ASTERISK $> []) <|> sepBy expression (keyword COMMA)

-- | CASE expression parser
caseExpr :: TokenParser Expression
caseExpr = do
    keyword CASE

    -- WHEN ... THEN ... pairs
    whenThens <- many1 $ do
        keyword WHEN
        whenExpr <- expression
        keyword THEN
        thenExpr <- expression
        return (whenExpr, thenExpr)

    -- ELSE clause (optional)
    elseExpr <- optionMaybe $ do
        keyword ELSE
        expression

    keyword END

    return $ CaseExpr whenThens elseExpr

-- | EXISTS expression parser
existsExpr :: TokenParser Expression
existsExpr = do
    keyword EXISTS
    keyword LEFT_PAREN
    query <- selectStatement
    keyword RIGHT_PAREN
    return $ ExistsExpr query

-- | Subquery expression parser
subQueryExpr :: TokenParser Expression
subQueryExpr = do
    keyword LEFT_PAREN
    query <- selectStatement
    keyword RIGHT_PAREN
    return $ SubQueryExpr query

-- | ORDER BY expression parser
orderByExpression :: TokenParser OrderByExpr
orderByExpression = do
    expr <- expression

    -- ASC/DESC (optional, default ASC)
    dir <- option Ascending $ 
        (keyword ASC $> Ascending) <|>
        (keyword DESC $> Descending)

    return $ OrderByExpr
        { orderExpr = expr
        , orderDir = dir
        }
