tail' :: [a] -> [a]
tail' (x:xs) = xs
tail' [] = error "Empty list"
