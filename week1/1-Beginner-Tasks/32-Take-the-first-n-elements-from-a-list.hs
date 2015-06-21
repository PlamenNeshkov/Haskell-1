take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) | n < 0 = error "Can't drop negative elements"
               | otherwise = x : take' (n-1) xs
