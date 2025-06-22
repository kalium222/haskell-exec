-- Ex. 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x-2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x y -> (x-2)*y) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
    let
        getSequence :: Integer -> [Integer]
        getSequence x
            | x <= 1 = [0]
            | even x = x : getSequence (div x 2)
            | otherwise = x : getSequence (3 * x + 1)
    in
        sum . filter even . getSequence 

-- Ex. 2

