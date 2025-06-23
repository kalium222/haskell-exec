data ExprT a = Lit a
    | Add (ExprT a) (ExprT a)
    | Mul (ExprT a) (ExprT a)
    deriving (Show, Eq)

