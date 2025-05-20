module Lamia.Parser.Parser
  ( parseSQL
  , runSqlParser
  , ParserError
  ) where

import Control.Monad (void)
import Data.Functor (($>))
import Control.Applicative ((<*))
import qualified Text.Parsec as P
import Text.Parsec ((<|>), many1, option, optionMaybe, sepBy, sepBy1, try, eof, chainl1)
import qualified Text.Parsec.Combinator as P
import Text.Parsec.Pos (newPos)
import qualified Text.Parsec.Pos as Pos

import Lamia.AST
import Lamia.Lexer.Token

type TokenParser = P.Parsec [Token] ()
type ParserError = P.ParseError

-- | Parse SQL
parseSQL :: [Token] -> Either ParserError SQLStatement
parseSQL = P.runParser sqlStmt () "<input>"

-- | Run parser
runSqlParser :: [Token] -> Either ParserError SQLStatement
runSqlParser = parseSQL

-- | SQL statement parser
sqlStmt :: TokenParser SQLStatement
sqlStmt = selectStmt <* eof
  where
    selectStmt = SelectStmt <$> selectStatement

-- | Token condition check
satisfyToken :: (TokenType -> Bool) -> (String -> Maybe a) -> TokenParser a
satisfyToken typePred extract = P.token showToken posFromToken testToken
  where
    showToken t = show (tokenType t) ++ " " ++ tokenValue t
    posFromToken t = newPos "<input>" (tokenLine t) (tokenPos t)
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

-- | SELECT statement parser
selectStatement :: TokenParser SelectStatement
selectStatement = do
  -- SELECT [DISTINCT]
  keyword SELECT
  distinct <- option False (keyword DISTINCT $> True)
  
  -- select_list
  columns <- parseSelectList
  
  -- FROM clause (optional)
  fromClause <- optionMaybe $ do
    keyword FROM
    tableExpr
  
  -- WHERE clause (optional)
  whereClause <- optionMaybe $ do
    keyword WHERE
    expression
  
  -- GROUP BY clause (optional)
  groupByClause <- option [] $ do
    keyword GROUP
    keyword BY
    sepBy1 expression (keyword COMMA)
  
  -- HAVING clause (optional)
  havingClause <- optionMaybe $ do
    keyword HAVING
    expression
  
  -- ORDER BY clause (optional)
  orderByClause <- option [] $ do
    keyword ORDER
    keyword BY
    sepBy1 orderByExpression (keyword COMMA)
  
  -- LIMIT clause (optional)
  limitClause <- optionMaybe $ do
    keyword LIMIT
    read <$> numericLiteral
  
  -- OFFSET clause (optional)
  offsetClause <- optionMaybe $ do
    keyword OFFSET
    read <$> numericLiteral
  
  return SelectStatement
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
parseSelectList :: TokenParser [(Expression, Maybe String)]
parseSelectList = sepBy1 selectItem (keyword COMMA)

-- | Select item parser
selectItem :: TokenParser (Expression, Maybe String)
selectItem = do
  expr <- expression
  
  -- AS alias (optional)
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

-- | Table name reference parser
tableName :: TokenParser TableExpr
tableName = do
  name <- identifier
  
  -- AS alias (optional)
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
  
  -- AS alias (required for subqueries)
  keyword AS
  alias <- identifier
  
  return $ TableSubQuery query alias

-- | JOIN expression parser
tableJoin :: TokenParser TableExpr
tableJoin = do
  left <- tableName <|> tableSubQuery
  
  -- Parse the join type and right side
  joinType <- joinTypeParser
  right <- tableName <|> tableSubQuery
  
  -- ON condition
  keyword ON
  onExpr <- expression
  
  return $ TableJoin $ JoinClause
    { joinLeft = left
    , joinType = joinType
    , joinRight = right
    , joinOn = onExpr
    }

-- | JOIN type parser
joinTypeParser :: TokenParser JoinType
joinTypeParser =
      (keyword INNER >> keyword JOIN $> InnerJoin)
  <|> (keyword LEFT >> option () (keyword OUTER) >> keyword JOIN $> LeftJoin)
  <|> (keyword RIGHT >> option () (keyword OUTER) >> keyword JOIN $> RightJoin)
  <|> (keyword CROSS >> keyword JOIN $> CrossJoin)
  <|> (keyword JOIN $> InnerJoin) -- Default is INNER JOIN

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
  
  -- Regular comparison operators
  option expr $ do
    op <- comparisonOp
    rhs <- additiveExpr
    return $ BinaryExpr op expr rhs
  
  where
    comparisonOp =
          (tokenOfType EQUAL $> Equal)
      <|> (tokenOfType NOT_EQUAL $> NotEqual)
      <|> (tokenOfType GREATER $> GreaterThan)
      <|> (tokenOfType LESS $> LessThan)
      <|> (tokenOfType GREATER_EQUAL $> GreaterEqual)
      <|> (tokenOfType LESS_EQUAL $> LessEqual)

-- | Addition/subtraction expression parser
additiveExpr :: TokenParser Expression
additiveExpr = chainl1 multiplicativeExpr addOp
  where
    addOp =
          (tokenOfType PLUS $> (\lhs rhs -> BinaryExpr Plus lhs rhs))
      <|> (tokenOfType MINUS $> (\lhs rhs -> BinaryExpr Minus lhs rhs))

-- | Multiplication/division expression parser
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

-- | Column reference parser
columnRefExpr :: TokenParser Expression
columnRefExpr = do
  -- ident1.ident2 or just ident
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

-- | Function call parser
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