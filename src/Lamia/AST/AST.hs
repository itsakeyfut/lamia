module Lamia.AST.AST
  ( SQLStatement(..)
  , SelectStatement(..)
  , TableExpr(..)
  , JoinType(..)
  , JoinClause(..)
  , Expression(..)
  , BinaryOp(..)
  , Literal(..)
  , OrderByExpr(..)
  , SortOrder(..)
  ) where

-- | SQL statement types
data SQLStatement
  = SelectStmt SelectStatement
  | InsertStmt -- Placeholder for future extension
  | UpdateStmt -- Placeholder for future extension
  | DeleteStmt -- Placeholder for future extension
  | CreateTableStmt -- Placeholder for future extension
  | AlterTableStmt -- Placeholder for future extension
  | DropTableStmt -- Placeholder for future extension
  deriving (Show, Eq)

-- | SELECT statement structure
data SelectStatement = SelectStatement
  { selectDistinct :: Bool                -- ^ Whether DISTINCT keyword is used
  , selectList     :: [(Expression, Maybe String)] -- ^ Selected columns (expression and alias)
  , selectFrom     :: Maybe TableExpr     -- ^ FROM clause
  , selectWhere    :: Maybe Expression    -- ^ WHERE clause
  , selectGroupBy  :: [Expression]        -- ^ GROUP BY clause
  , selectHaving   :: Maybe Expression    -- ^ HAVING clause
  , selectOrderBy  :: [OrderByExpr]       -- ^ ORDER BY clause
  , selectLimit    :: Maybe Integer       -- ^ LIMIT clause
  , selectOffset   :: Maybe Integer       -- ^ OFFSET clause
  } deriving (Show, Eq)

-- | Table expression
data TableExpr
  = TableName String (Maybe String)       -- ^ Table name and alias
  | TableJoin JoinClause                  -- ^ JOIN operation
  | TableSubQuery SelectStatement String  -- ^ Subquery and alias
  deriving (Show, Eq)

-- | JOIN types
data JoinType
  = InnerJoin
  | LeftJoin
  | RightJoin
  | FullJoin
  | CrossJoin
  deriving (Show, Eq)

-- | JOIN clause
data JoinClause = JoinClause
  { joinLeft  :: TableExpr    -- ^ Left table expression
  , joinType  :: JoinType     -- ^ JOIN type
  , joinRight :: TableExpr    -- ^ Right table expression
  , joinOn    :: Expression   -- ^ ON condition
  } deriving (Show, Eq)

-- | Expression
data Expression
  = BinaryExpr BinaryOp Expression Expression  -- ^ Binary operation
  | UnaryExpr String Expression                -- ^ Unary operation
  | LiteralExpr Literal                        -- ^ Literal
  | ColumnRef String (Maybe String)            -- ^ Column reference (column name and table name)
  | FunctionCall String [Expression]           -- ^ Function call
  | CaseExpr [(Expression, Expression)] (Maybe Expression) -- ^ CASE expression
  | ExistsExpr SelectStatement                 -- ^ EXISTS expression
  | SubQueryExpr SelectStatement               -- ^ Subquery
  deriving (Show, Eq)

-- | Binary operators
data BinaryOp
  = And | Or                          -- ^ Logical operators
  | Equal | NotEqual                  -- ^ Equality operators
  | LessThan | GreaterThan            -- ^ Comparison operators
  | LessEqual | GreaterEqual          -- ^ Comparison operators
  | Plus | Minus | Multiply | Divide  -- ^ Arithmetic operators
  | Like | NotLike                    -- ^ LIKE operators
  | In | NotIn                        -- ^ IN operators
  deriving (Show, Eq)

-- | Literal values
data Literal
  = StringLit String    -- ^ String literal
  | NumericLit String   -- ^ Numeric literal
  | BoolLit Bool        -- ^ Boolean literal
  | NullLit             -- ^ NULL value
  deriving (Show, Eq)

-- | ORDER BY clause element
data OrderByExpr = OrderByExpr
  { orderExpr  :: Expression  -- ^ Sorting expression
  , orderDir   :: SortOrder   -- ^ Sorting direction
  } deriving (Show, Eq)

-- | Sorting direction
data SortOrder
  = Ascending   -- ^ Ascending order
  | Descending  -- ^ Descending order
  deriving (Show, Eq)