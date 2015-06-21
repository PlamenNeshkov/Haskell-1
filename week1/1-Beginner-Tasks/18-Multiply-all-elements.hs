mult :: (Num a) => a -> [a] -> [a]
mult _ [] = []
mult n (x:xs) = x * n : mult n xs
