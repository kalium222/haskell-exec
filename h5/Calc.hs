{-# LANGUAGE AllowAmbiguousTypes #-}
import ExprT

-- Ex. 1
eval :: ExprT -> Integer
eval (Lit num) = num
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- Ex. 2
evalStr :: String -> Maybe Integer
evalStr s = case parseExp s of
    Nothing -> Nothing
    (Just e) -> Just (eval e)

-- Ex. 3
class Expr expType inType where
    lit :: inType -> expType
    add :: expType -> expType -> expType
    mul :: expType -> expType -> expType

instance Expr ExprT Integer where
    lit = Lit
    add = Add
    mul = Mul

-- Ex. 4
instance Expr Integer Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool Integer where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax= MinMax Integer deriving (Eq, Show)
newtype Mod7= Mod7 Integer deriving (Eq, Show)

instance Expr MinMax Integer where
  lit = MinMax
  add (MinMax m1) (MinMax m2) = MinMax (max m1 m2)
  mul (MinMax m1) (MinMax m2) = MinMax (min m1 m2)

instance Expr Mod7 Integer where
  lit = Mod7 . flip mod 7
  add (Mod7 a) (Mod7 b) = Mod7 ( (a+b) `mod` 7 )
  mul (Mod7 a) (Mod7 b) = Mod7 ( (a*b) `mod` 7 )
