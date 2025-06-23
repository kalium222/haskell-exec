module ExprT where

data ExprT = Lit Integer
    | Add ExprT ExprT
    | Mul ExprT ExprT
    deriving (Show, Eq)

-- Just assume this function works
parseExp :: String -> Maybe exp
parseExp _ = Nothing
