{-# OPTIONS_GHC -Wall #-}

module Golf where

-- Ex. 1
skips :: [a] -> [[a]]
-- skips l =
--     let
--         l_ind = zip l [1..]
--         skips_helper :: [(a, Int)] -> [(a, Int)]
--         skips_helper [] = []
--         skips_helper l_@((_, n):_) = filter ( \(_, index)-> mod index n == 0) l_
--
--         skips_recur :: [(a, Int)] ->[[a]]
--         skips_recur [] = []
--         skips_recur l_ind_@(_:xs) = map fst (skips_helper l_ind_) : skips_recur xs
--     in
--         skips_recur l_ind

-- NOTE: short version
-- 就离谱
skips l = [ [ e | (i, e) <- zip [1..] l, i `mod` n == 0 ] | n <- [1..length l]]

-- Ex. 2
localMaxima :: [Integer] -> [Integer]
-- localMaxima l = [ l!!n | n<-[1..(length l-2)], l!!(n-1)<l!!n, l!!(n+1)<l!!n ]
-- NOTE: without (!!)
-- 就离谱
localMaxima l = [ y | (x, y, z) <- zip3 l (drop 1 l) (drop 2 l), x < y, z < y ]

-- Ex. 3
-- TODO:
histogram :: [Integer] -> String
histogram l =
    let
        bar = "\n==========\n0123456789\n"
    in
        "    *     " ++ bar
