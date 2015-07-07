module PolymorphicList where

data List a = Cons a (List a) | Empty
  deriving (Show)

type IntList = List Int

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _      = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _      = Nothing

safeNth :: Int -> [a] -> Maybe a
safeNth 0 (x:_)  = Just x
safeNth n (x:xs) = safeNth (n-1) xs
safeNth _ _      = Nothing
