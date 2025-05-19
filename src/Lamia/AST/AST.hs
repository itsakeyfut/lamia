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

-- | SQL Statement
data SQLStatement
    = SelectStmt SelectStatement
    -- Placeholder
    | InsertStmt
    | UpdateStmt
    | DeleteStmt
    | CreateStmt
    | AlterTableStmt
    | DropTableStmt
    deriving (Show, Eq)

-- | Select Statement
data SelectStatement = SelectStatement
    { selectDistinct :: Bool                -- ^ DISTINCT
    , selectList     :: [(Expression, Maybe String)] -- ^ SELECT
    , selectFrom     :: Maybe TableExpr     -- ^ FROM
    , selectWhere    :: Maybe Expression    -- ^ WHERE
    , selectGroupBy  :: [Expression]        -- ^ GROUP BY
    , selectHaving   :: Maybe Expression    -- ^ HAVING
    , selectOrderBy  :: [OrderByExpr]       -- ^ ORDER BY
    , selectLimit    :: Maybe Integer       -- ^ LIMIT
    , selectOffset   :: Maybe Integer       -- ^ OFFSET
    } deriving (Show, Eq)

-- | Table Expression
data TableExpr
    = TableName String (Maybe String)      -- ^ Table name and alias
    | TableJoin JoinClause                 -- ^ JOIN
    | TableSubQuery SelectStatement String -- ^ Subquery and alias
    deriving (Show, Eq)

-- | Join Type
data JoinType
    = InnerJoin
    | LeftJoin
    | RightJoin
    | FullJoin
    | CrossJoin
    deriving (Show, Eq)

-- | Join Clause
data JoinClause = JoinClause
    { joinLeft  :: TableExpr  -- ^ Left table
    , joinType  :: JoinType   -- ^ Join type
    , joinRight :: TableExpr  -- ^ Right table
    , joinOn    :: Expression -- ^ ON condition
    } deriving (Show, Eq)

-- | Expression
data Expression
    = BinaryExpr BinaryOp Expression Expression              -- ^ Binary expression
    | UnaryExpr String Expression                            -- ^ Unary expression
    | LiteralExpr Literal                                    -- ^ Literal value
    | ColumnRef String (Maybe String)                        -- ^ Column reference (table, column)
    | FunctionCall String [Expression]                       -- ^ Function call
    | CaseExpr [(Expression, Expression)] (Maybe Expression) -- ^ CASE expression
    | ExistsExpr SelectStatement                             -- ^ EXISTS subquery
    | SubQueryExpr SelectStatement                           -- ^ Subquery
    deriving (Show, Eq)

-- | Binary Operator
data BinaryOp
    = Add | Or -- ^ Logical
    | Equal | NotEqual -- ^ Euality
    | LessThan | GreaterThan -- ^ Comparison
    | LessEqual | GreaterEqual -- ^ Comparison
    | Plus | Minus | Multiply | Divide -- ^ Arithmetic
    | Like | NotLike -- ^ Like
    | In | NotIn -- ^ In
    deriving (Show, Eq)

-- | Literal Value
data Literal
    = StringLit String  -- ^ String literal
    | NumericLit String -- ^ Numeric literal
    | BoolLit Bool      -- ^ Boolean literal
    | NullLit           -- ^ NULL literal
    deriving (Show, Eq)

-- | Order By Expression
data OrderByExpr = OrderByExpr
    { orderExpr :: Expression -- ^ ORDER BY
    , orderDir  :: SortOrder  -- ^ Sort direction
    } deriving (Show, Eq)

-- | Sort Order
data SortOrder
    = Ascending  -- ^ ASC
    | Descending -- ^ DESC
    deriving (Show, Eq)
