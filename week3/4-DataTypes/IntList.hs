module IntList where

data IntList = Cons Int IntList | Empty
  deriving (Show)

fromList :: [Int] -> IntList
fromList (x:xs) = Cons x (fromList xs)
fromList _ = Empty

toList :: IntList -> [Int]
toList (Cons x xs) = x : toList xs
toList _ = []
