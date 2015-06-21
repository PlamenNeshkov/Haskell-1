drop' :: Int -> [a] -> [a]
drop' 0 x = x
drop' n (x:xs) | n < 0 = error "Can't drop negative elements"
               | otherwise = drop' (n-1) xs
