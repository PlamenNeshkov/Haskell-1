--01. Map as foldl
mapAsFoldl :: (a -> b) -> [a] -> [b]
mapAsFoldl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

--02. Filter as foldl
--filterAsFoldl :: (a -> b) -> [a] -> [b]
filterAsFoldl f xs = foldl p [] xs
                     where p = \acc x -> if f x then acc ++ [x] else acc

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
row :: Int -> [[a]] -> [a]
row 0 (x:_) = x
row n (x:xs) = row (n-1) xs
row _ _ = []

--12. Transpose a matrix

--13. Sum of matrices
sumMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
sumMatrices (x:xs) (y:ys) = zipWith (+) x y : sumMatrices xs ys
sumMatrices _ _ = []

--14. Multiply matrices
-- probably wrong
multMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
multMatrices (x:xs) (y:ys) = zipWith (*) x y : multMatrices xs ys
multMatrices _ _ = []

--15. Histogram
