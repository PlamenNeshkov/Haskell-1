--01. Map as foldl
--map' :: (c -> d) -> [c] -> [d]
--map' f (x:xs) = foldl'  : map' f xs
--  where foldl' :: (a -> b -> a) -> a -> [b] -> a
--        foldl' _ acc [] = acc
--        foldl' f acc (x:xs) = foldl f (f acc x) xs
--map' _ _ = []

--foldl :: ([d] -> c -> [d]) -> [d] -> [c] -> [d]

--03. Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<=x) xs) ++ [x] ++ quicksort (filter (>x) xs)

--04. Repeat
repeat' :: a -> [a]
repeat' x = x : repeat' x

--05. Cycling
cycle' :: [a] -> [[a]]
cycle' x = x : cycle' x

--06. Take every nth element from a list
every :: Integral a => a -> [a1] -> [(a1,a)]
every n (x:xs) = filter (\y -> snd y `mod` n == 0) (y:ys)
                  where (y:ys) = zip (x:xs) [1..]
every _ _ = []

--07. Get the local maximas in a list of Integers
localMaxima :: Ord a => [a] -> [a]
localMaxima (x:y:z:zs) | y > x && y > z = y : lMax
                       | otherwise = lMax
                          where lMax = localMaxima (y:z:zs)
localMaxima _ = []

--08. Map a function to a list of lists
mapMap :: (a -> b) -> [[a]] -> [[b]]
mapMap f (x:xs) = map f x : mapMap f xs
mapMap _ _ = []

--09. Filter a list of lists
filterFilter :: (a -> Bool) -> [[a]] -> [[a]]
filterFilter p (x:xs) = filter p x : filterFilter p xs
filterFilter _ _ = []

--10. Generate the unit matrix by given element and dimensions
unit :: (Enum a, Num a) => a -> Int -> [[a]]
unit elem dim = map f [0..dim-1]
	where f pos = (take pos [0, 0..]) ++ [elem] ++ (take (dim-pos-1) [0, 0..])

--11. Get the nth row and column of a matrix

--12. Transpose a matrix

--13. Sum of matrices

sumMatrices (x:xs) (y:ys) = zipWith (+) x y : sumMatrices xs ys
sumMatrices _ _ = []

--14. Multiply matrices

--15. Histogram
