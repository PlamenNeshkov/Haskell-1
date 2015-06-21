nth :: (Num a1, Ord a1) => a1 -> [[a]] -> [a]
nth _ [] = []
nth 0 (x:_) = x
nth n (x:xs) | n < 0 = error "undefined"
             | otherwise = nth (n-1) xs
