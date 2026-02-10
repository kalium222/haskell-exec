{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Ex. 1
fib :: Integer -> Integer
fib x
  | x == 0 = 0
  | x == 1 = 1
  | x >= 2 = fib (x - 1) + fib (x - 2)

fib1 :: [Integer]
fib1 = [fib x | x <- [0 ..]]

-- Ex. 2

-- test :: Integer -> [Integer]
-- test n | n == 0 = [0]
--   | n == 1 = [0, 1]
--   | n >= 2 = pre_l ++ [pre_l !! (m - 1) + pre_l !! (m - 2)]
--   where
--     pre_l = test $ n - 1
--     m = fromIntegral n

-- NOTE: 离谱
fib2 :: [Integer]
fib2 = 0 : 1 : zipWith (+) fib2 (tail fib2)

-- NOTE: 离谱
fib3 :: [Integer]
fib3 = 0 : scanl (+) 1 fib3

-- Ex. 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance (Show a) => Show (Stream a) where
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

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)

-- NOTE: 离谱
ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+ 1) ruler)

-- ruler :: Stream Integer
-- ruler = streamMap h $ streamFromSeed (+ 1) 1
--
-- h :: Integer -> Integer
-- h x = y
--   where
--     y = maximum [a | a <- [0 .. x], mod x (2 ^ a) == 0]

-- Ex. 6
x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  (Stream a as) + (Stream b bs) = Stream (a + b) (as + bs)
  (Stream a as) * s2@(Stream b bs) = Stream (a * b) (streamMap (* a) bs + as * s2)
  fromInteger n = Stream n $ streamRepeat 0
  negate = streamMap negate

instance Fractional (Stream Integer) where
  (/) :: Stream Integer -> Stream Integer -> Stream Integer
  s1@(Stream a as) / s2@(Stream b bs) = Stream (div a b) $ streamMap (`div` b) (as - (s1 / s2) * bs)

fib4 :: Stream Integer
fib4 = x / (1 - x - x * x)

-- NOTE: this is fucking astounishing

-- Ex. 7
data Matrix2x2 = Matrix2x2 Integer Integer Integer Integer

instance Num Matrix2x2 where
  (Matrix2x2 a b c d) * (Matrix2x2 e f g h) = Matrix2x2 (a * e + b * g) (a * f + b * h) (c * e + d * g) (c * f + d * h)

fib5 :: Integer -> Integer
fib5 n =
  let Matrix2x2 _ _ _ x = (Matrix2x2 1 1 1 0) ^ n
   in x
