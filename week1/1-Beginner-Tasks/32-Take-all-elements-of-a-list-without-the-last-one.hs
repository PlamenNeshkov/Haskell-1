init' :: [a] -> [a]
init' [] = error "Empty list"
init' (_:[]) = []
init' (x:xs) = x : init' xs
