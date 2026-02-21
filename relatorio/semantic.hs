type Env = Map Id Type

data SemanticError
  = UnboundVariable Id String  -- Variable name and context
  | UnboundFunction Id String  -- Function name and context
  | TypeMismatch Type Type String  -- Expected, Actual and context
  deriving (Show)

type Checker a = ReaderT Env (Except SemanticError) a