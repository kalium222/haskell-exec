-- Ex. 1
toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | n < 10 = [n]
  | otherwise = (n `mod` 10) : toDigitsRev (div (n - mod n 10) 10)

-- Ex. 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l
  | length l < 2 = l
doubleEveryOther (x : rest)
  | even (length rest) = x : doubleEveryOther rest
  | otherwise = 2 * x : doubleEveryOther rest

-- Ex. 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [n] = n
sumDigits (x : rest) = sum (toDigits x) + sumDigits rest

-- Ex. 4
validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

-- Ex. 5
type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a
