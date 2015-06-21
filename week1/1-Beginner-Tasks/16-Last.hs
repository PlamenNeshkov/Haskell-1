last' :: [a] -> a
last' (x:xs) = last xs
last' [] = error "Empty list"
