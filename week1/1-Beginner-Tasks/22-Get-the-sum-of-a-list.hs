sum' :: (Num a) => [a] -> a
sum' (x:xs) = x + sum xs
