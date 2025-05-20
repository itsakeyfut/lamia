module Lamia.Evaluator.Evaluator
  ( evaluateAst
  ) where

import Data.List (intercalate)
import Lamia.AST

-- | Evaluate abstract syntax tree
evaluateAst :: SQLStatement -> String
evaluateAst stmt = case stmt of
  SelectStmt select -> evaluateSelect select
  _ -> "Unimplemented SQL statement."

-- | Evaluate SELECT statement
evaluateSelect :: SelectStatement -> String
evaluateSelect select = 
  let header = "SELECT statement evaluation result:\n"
      distinctInfo = if selectDistinct select then "DISTINCT is specified\n" else ""
      columnsInfo = "Selected columns: " ++ formatSelectList (selectList select) ++ "\n"
      fromInfo = case selectFrom select of
        Just tableExpr -> "FROM clause: " ++ formatTableExpr tableExpr ++ "\n"
        Nothing -> ""
      whereInfo = case selectWhere select of
        Just expr -> "WHERE clause: " ++ formatExpr expr ++ "\n"
        Nothing -> ""
      groupByInfo = if null (selectGroupBy select) 
                    then "" 
                    else "GROUP BY clause: " ++ intercalate ", " (map formatExpr (selectGroupBy select)) ++ "\n"
      havingInfo = case selectHaving select of
        Just expr -> "HAVING clause: " ++ formatExpr expr ++ "\n"
        Nothing -> ""
      orderByInfo = if null (selectOrderBy select)
                    then ""
                    else "ORDER BY clause: " ++ intercalate ", " (map formatOrderBy (selectOrderBy select)) ++ "\n"
      limitInfo = case selectLimit select of
        Just limit -> "LIMIT: " ++ show limit ++ "\n"
        Nothing -> ""
      offsetInfo = case selectOffset select of
        Just offset -> "OFFSET: " ++ show offset ++ "\n"
        Nothing -> ""
  in header ++ distinctInfo ++ columnsInfo ++ fromInfo ++ whereInfo ++ 
     groupByInfo ++ havingInfo ++ orderByInfo ++ limitInfo ++ offsetInfo ++
     "\n(Actual query execution is not implemented. This only displays the AST analysis result.)"

-- | Format SELECT list
formatSelectList :: [(Expression, Maybe String)] -> String
formatSelectList = intercalate ", " . map formatSelectItem
  where
    formatSelectItem (expr, maybeAlias) =
      formatExpr expr ++ case maybeAlias of
        Just alias -> " AS " ++ alias
        Nothing -> ""

-- | Format table expression
formatTableExpr :: TableExpr -> String
formatTableExpr expr = case expr of
  TableName name maybeAlias ->
    name ++ case maybeAlias of
      Just alias -> " AS " ++ alias
      Nothing -> ""
  
  TableJoin joinClause ->
    formatTableExpr (joinLeft joinClause) ++ " " ++
    formatJoinType (joinType joinClause) ++ " " ++
    formatTableExpr (joinRight joinClause) ++ " ON " ++
    formatExpr (joinOn joinClause)
  
  TableSubQuery select alias ->
    "(" ++ evaluateSelect select ++ ") AS " ++ alias

-- | Format join type
formatJoinType :: JoinType -> String
formatJoinType jt = case jt of  -- Renamed to avoid name shadowing
  InnerJoin -> "INNER JOIN"
  LeftJoin -> "LEFT JOIN"
  RightJoin -> "RIGHT JOIN"
  FullJoin -> "FULL JOIN"
  CrossJoin -> "CROSS JOIN"

-- | Format expression
formatExpr :: Expression -> String
formatExpr expr = case expr of
  BinaryExpr op left right ->
    "(" ++ formatExpr left ++ " " ++ formatBinaryOp op ++ " " ++ formatExpr right ++ ")"
  
  UnaryExpr op expr' ->
    op ++ "(" ++ formatExpr expr' ++ ")"
  
  LiteralExpr lit ->
    formatLiteral lit
  
  ColumnRef col maybeTable ->
    case maybeTable of
      Just table -> table ++ "." ++ col
      Nothing -> col
  
  FunctionCall funcName args ->
    funcName ++ "(" ++ intercalate ", " (map formatExpr args) ++ ")"
  
  CaseExpr whenThens maybeElse ->
    "CASE " ++
    concatMap (\(whenExpr, thenExpr) -> 
        "WHEN " ++ formatExpr whenExpr ++ " THEN " ++ formatExpr thenExpr ++ " "
      ) whenThens ++
    (case maybeElse of
        Just elseExpr -> "ELSE " ++ formatExpr elseExpr ++ " "
        Nothing -> "") ++
    "END"
  
  ExistsExpr select ->
    "EXISTS (" ++ evaluateSelect select ++ ")"
  
  SubQueryExpr select ->
    "(" ++ evaluateSelect select ++ ")"

-- | Format binary operator
formatBinaryOp :: BinaryOp -> String
formatBinaryOp op = case op of
  And -> "AND"
  Or -> "OR"
  Equal -> "="
  NotEqual -> "<>"
  LessThan -> "<"
  GreaterThan -> ">"
  LessEqual -> "<="
  GreaterEqual -> ">="
  Plus -> "+"
  Minus -> "-"
  Multiply -> "*"
  Divide -> "/"
  Like -> "LIKE"
  NotLike -> "NOT LIKE"
  In -> "IN"
  NotIn -> "NOT IN"

-- | Format literal
formatLiteral :: Literal -> String
formatLiteral lit = case lit of
  StringLit s -> "'" ++ s ++ "'"
  NumericLit n -> n
  BoolLit True -> "TRUE"
  BoolLit False -> "FALSE"
  NullLit -> "NULL"

-- | Format ORDER BY expression
formatOrderBy :: OrderByExpr -> String
formatOrderBy orderBy =
  formatExpr (orderExpr orderBy) ++ " " ++
  case orderDir orderBy of
    Ascending -> "ASC"
    Descending -> "DESC"