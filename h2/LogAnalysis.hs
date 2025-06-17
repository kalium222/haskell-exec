{-# OPTIONS_GHC -Wall -dynamic #-}
module LogAnalysis where

import Log
import Data.List.Split (splitOn)
-- Ex. 1
parseMessage :: String -> LogMessage
parseMessage s = parseMessageStringList (splitOn " " s)

parseMessageStringList :: [String] -> LogMessage
parseMessageStringList ("E":e_s:ts_s:rest) =
    let e = read e_s :: Int
        ts = read ts_s :: Int
        info_s = unwords rest
    in LogMessage (Error e) ts info_s
parseMessageStringList ("I":ts_s:rest) =
    let ts = read ts_s :: Int
        info_s = unwords rest
    in LogMessage Info ts info_s
parseMessageStringList ("W":ts_s:rest) =
    let ts = read ts_s :: Int
        info_s = unwords rest
    in LogMessage Warning ts info_s
parseMessageStringList s = Unknown (unwords s)

parse :: String -> [LogMessage]
parse s = map parseMessage (splitOn "\n" s)

-- Ex. 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m@(LogMessage _ ts _) tree =
    case tree of
        Leaf -> Node Leaf m Leaf
        Node left_tree _m@(LogMessage _ _ts _) right_tree ->
            if _ts < ts then Node left_tree _m (insert m right_tree)
            else Node (insert m left_tree) _m right_tree
        _ -> tree

-- Ex. 3
build :: [LogMessage] -> MessageTree
build [] = Leaf
build [m] = Node Leaf m Leaf
build (m:rest) =
    insert m (build rest)

-- Ex. 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left_tree m right_tree) =
    inOrder left_tree ++ [m] ++ inOrder right_tree

-- Ex. 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong l =
    let
        isSevere :: LogMessage -> Bool
        isSevere (LogMessage (Error e) _ _) = e > 50
        isSevere _ = False
        getInfo :: LogMessage -> String
        getInfo (LogMessage _ _ s) = s
        getInfo (Unknown s) = s
        _l = filter isSevere l
    in 
        map getInfo _l
