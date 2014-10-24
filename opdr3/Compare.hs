module Compare where
import Expression

data Relop = LessThan | LessEqual | Equal | GreaterEqual | Greater | NotEqual deriving (Show)
data Comparison = Cmp Relop Expr Expr deriving (Show)
