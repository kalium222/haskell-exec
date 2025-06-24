import Data.List (foldl')

-- Ex. 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x y -> (x - 2) * y) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
  let getSequence :: Integer -> [Integer]
      getSequence x
        | x <= 1 = [0]
        | even x = x : getSequence (div x 2)
        | otherwise = x : getSequence (3 * x + 1)
   in sum . filter even . getSequence

-- Ex. 2
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldl' insert Leaf

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insert :: Tree a -> a -> Tree a
insert Leaf x = Node 0 Leaf x Leaf
insert (Node h l_tree val r_tree) x
  | height l_tree >= height r_tree =
      let r_tree' = insert r_tree x
          h' = 1 + max (height l_tree) (height r_tree')
       in Node h' l_tree val r_tree'
  | otherwise =
      let l_tree' = insert l_tree x
          h' = 1 + max (height l_tree') (height r_tree)
       in Node h' l_tree' val r_tree

recurHeight :: Tree a -> Integer
recurHeight Leaf = -1
recurHeight (Node _ l _ r) = max (recurHeight l) (recurHeight r) + 1

isBalanceTree :: Tree a -> Bool
isBalanceTree Leaf = True
isBalanceTree (Node _ l _ r) =
  let h_l = recurHeight l
      h_r = recurHeight r
      delta = h_l - h_r
   in -1 <= delta
        && delta <= 1
        && isBalanceTree l
        && isBalanceTree r

-- Ex. 3
xor :: [Bool] -> Bool
xor = foldr (\x res -> if x then not res else res) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x res -> f x : res) []

-- NOTE: 这是人？
foldl'' :: (a -> b -> a) -> a -> [b] -> a
foldl'' f base xs = h base
  where
    -- h = foldr (\x acc->(\a->acc (f a x))) id xs
    h = foldr (\x acc a -> acc (f a x)) id xs

-- Ex. 4
-- NOTE: ?
sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let l = [i + j + 2 * i * j | j <- [1 .. n], i <- [1 .. j]]
   in map (\x -> 2 * x + 1) (filter (`notElem` l) [1 .. n])
