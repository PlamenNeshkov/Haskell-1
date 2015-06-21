head' :: [a] -> a
head' (x:xs) = x
head' [] = error "Empty list"
