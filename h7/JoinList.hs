import Sized
import Scrabble (Score, scoreString)

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Ex. 1

tag :: (Monoid m) => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append _ jl1 jl2) = (tag jl1) <> (tag jl2)

(+++) :: (Monoid m) => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

-- Ex. 2
-- 1.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append _ jl1 jl2)
  | i < front_size = indexJ i jl1
  | otherwise = indexJ (i - front_size) jl2
  where
    front_size = getSize $ size $ tag jl1

-- 2.
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl | i <= 0 = jl
dropJ _ (Single _ _) = Empty
dropJ i (Append _ jl1 jl2)
  | i < front_size = (dropJ i jl1) +++ jl2
  | otherwise = dropJ (i - front_size) jl2
  where
    front_size = getSize $ size $ tag jl1

-- 3.
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i <= 0 = Empty
takeJ _ jl@(Single _ _) = jl
takeJ i (Append _ jl1 jl2)
  | i < front_size = takeJ i jl1
  | otherwise = jl1 +++ (takeJ (i - front_size) jl2)
  where
    front_size = getSize $ size $ tag jl1

-- Ex. 3

-- test
node a = Single (Size 1) a
tree = (node 'f' +++ node 'u') +++ (node 'c' +++ node 'k')

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
