-- Ex. 1
fib :: Integer -> Integer
fib x
  | x == 0 = 0
  | x == 1 = 1
  | x >= 2 = fib (x - 1) + fib (x - 2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0 ..]]

-- Ex. 2

-- test :: Integer -> [Integer]
-- test n
--   | n == 0 = [0]
--   | n == 1 = [0, 1]
--   | n >= 2 = pre_l ++ [pre_l !! (m - 1) + pre_l !! (m - 2)]
--   where
--     pre_l = test $ n - 1
--     m = fromIntegral n

-- NOTE: 离谱
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Ex. 3
-- NOTE: 😑😓🙄😅😰
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = unwords . take 5 . map show . streamToList

-- Ex. 4
streamRepeat :: a -> Stream a
streamRepeat a = Stream a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a0 = Stream a0 $ streamFromSeed f $ f a0

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = streamMap h (streamFromSeed (+ 1) 1)

h :: Integer -> Integer
h x = y
  where
    y = maximum [a | a <- [0 .. x], mod x (2 ^ a) == 0]
