group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:y:ys) | x == y = (x:[y]) : group' ys
                | otherwise = [x] : group' (y:ys)
