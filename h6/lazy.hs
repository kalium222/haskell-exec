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
