product' :: (Num a) => [a] -> a
product' (x:xs) = x * product xs
