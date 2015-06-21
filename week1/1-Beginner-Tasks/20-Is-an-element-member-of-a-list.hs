member :: (Eq a) => a -> [a] -> Bool
member num [] = False
member num (x:xs) = if num == x then True else member num xs
