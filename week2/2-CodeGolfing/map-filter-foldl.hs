map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x : map' f xs
map' _ _ = []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p (x:xs) | p x = x : filter' p xs
                 | otherwise = filter' p xs
filter' _ _ = []

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl f (f acc x) xs
