import ExprT
import Parser (parseExp)
import StackVM

eval :: ExprT -> Integer
eval (Lit num) = num
eval (ExprT.Add e1 e2) = eval e1 + eval e2
eval (ExprT.Mul e1 e2) = eval e1 * eval e2

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit ExprT.Add ExprT.Mul s of
  Nothing -> Nothing
  (Just e) -> Just (eval e)

-- Ex. 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

-- Ex. 4
testExp :: (Expr a) => Maybe a
testExp = parseExp lit add mul "(3* -4) + 5"

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (&&)
  mul = (||)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  add (Mod7 a) (Mod7 b) = Mod7 $ mod (a + b) 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ mod (a * b) 7

-- Ex. 5
instance Expr Program where
  lit a = [PushI a]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile s = parseExp lit add mul s :: Maybe Program
