--01. Even
even' :: Int -> Bool
even' x = mod x 2 == 0

--02. Odd
odd' :: Int -> Bool
odd' x = mod x 2 == 1

--03. Calculate BMI
bmi :: Float -> Float -> Float
bmi height weight = weight / ((height/100) ^ 2)

--04. Convert degrees to radians
deg2Rad :: Float -> Float
deg2Rad deg = deg / 180 * pi

--05. Convert radians to degrees
rad2Deg :: Float -> Float
rad2Deg rad = rad * 180 / pi

--06. Does it form a triangle?
isTriangle :: (Int, Int, Int) -> Bool
isTriangle (a, b, c) = a > 0 && b > 0 && c > 0 &&
                       (a + b) > c && (a + c) > b && (b + c) > a

--07. Perimeter of a triangle
perimeter :: (Float, Float, Float) -> Float
perimeter (a, b, c) = a + b + c

--08. Area of a triangle
area :: (Float, Float, Float) -> Float
area (a, b, c) = sqrt (p
	* (p - a)
	* (p - b)
	* (p - c))
	where p = (a + b + c) / 2

--09. Calculate
calculate :: Char -> Float -> Float -> Float
calculate op a b | op == '+' = a + b
				         | op == '-' = a - b
				         | op == '*' =  a * b
				         | op == '/' =  a / b
				         | otherwise = error "Unknown operator"

--10. Currency converter
convert :: String -> String -> Float -> Float
convert "bgn" "usd" value = value * 0.58;
convert "bgn" "eur" value = value * 0.51;
convert "usd" "bgn" value = value * 1.72;
convert "usd" "eur" value = value * 0.88;
convert "eur" "usd" value = value * 1.14;
convert "eur" "bgn" value = value * 1.96;
convert _     _     value = error "Invalid currencies"

--11. Type signatures
-- Added!

--12. Rewrite the triangle functions from above to get lists as arguments
-- Done with tuples

--13. Head
head' :: [a] -> a
head' (x:xs) = x
head' [] = error "Empty list"

--14. Tail
tail' :: [a] -> [a]
tail' (x:xs) = xs
tail' [] = error "Empty list"

--15. We love pattern matching
-- Rewritten.

--16. Last
last' :: [a] -> a
last' (x:xs) = last xs
last' [] = error "Empty list"

--17. Double all elements
double :: (Num a) => [a] -> [a]
double [] = []
double (x:xs) = x * 2 : double(xs)

--18. More generic
mult :: (Num a) => a -> [a] -> [a]
mult _ [] = []
mult n (x:xs) = x * n : mult n xs

--19. Get the n-th element of a list
nth :: (Num a1, Ord a1) => a1 -> [[a]] -> [a]
nth _ [] = []
nth 0 (x:_) = x
nth n (x:xs) | n < 0 = error "undefined"
             | otherwise = nth (n-1) xs

--20. Is an element member of a list?
member :: (Eq a) => a -> [a] -> Bool
member num [] = False
member num (x:xs) = if num == x then True else member num xs

--21. Is the list a fibonacci sequence?
isFibonacciSequence :: (Eq a, Num a) => [a] -> Bool
isFibonacciSequence (x:y:[]) = True
isFibonacciSequence (x:y:z:zs) = if z == y + x
                                  then isFibonacciSequence (y:z:zs)
                                  else False

--22. Get the sum of a list
sum' :: (Num a) => [a] -> a
sum' (x:xs) = x + sum xs

--23. Get the product of a list
product' :: (Num a) => [a] -> a
product' (x:xs) = x * product xs

--24. Multiply lists
multLists :: (Num a) => [a] -> [a] -> a
multLists (x:xs) (y:ys) = x * y + multLists xs ys

--25. Number to String
number2string :: Int -> String
number2string x = show x

--26. String to number
string2number :: String -> Int
string2number x = read x

--27. Is valid ID?
-- Not Done

--28. Get the zodiac sign from an ID
-- Not Done

--30. Concatenate the lists
concatenate :: [a] -> [a] -> [a]
concatenate x y = x ++ y

--31. Take all elements of a list without the last one
init' :: [a] -> [a]
init' [] = error "Empty list"
init' (_:[]) = []
init' (x:xs) = x : init' xs

--32. Take the first n elements from a list
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x:xs) | n < 0 = error "Can't drop negative elements"
               | otherwise = x : take' (n-1) xs

--33. Drop hte first n elements from a list
drop' :: Int -> [a] -> [a]
drop' 0 x = x
drop' n (x:xs) | n < 0 = error "Can't drop negative elements"
               | otherwise = drop' (n-1) xs

--34. Zipping lists
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

--35. Unzipping lists
unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([],[])
unzip' ((x, y):zs) = (x : fst pair, y : snd pair)
                      where pair = unzip' zs

--36. Grouping
group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:y:ys) | x == y = (x:[y]) : group' ys
                | otherwise = [x] : group' (y:ys)

--37. Generate all pythagorean triples
pyths :: Int -> Int -> [(Int, Int, Int)]
pyths from to = [(a, b, c) | a <- [from..to], b <- [from..to], c <- [from..to],
                a < b, b < c, (a^2 + b^2 == c^2)]

--38. Return a function, which multiplies a number by a factor
multiplyBy :: (Num a) => a -> a -> a
multiplyBy n = \x -> x * n

--39. Get the last digit of all numbers in a list
lastDigits :: (Integral a) => [a] -> [a]
lastDigits [] = []
lastDigits (x:xs) = if x > 9 then (mod x 10):lastDigits xs else x:lastDigits xs

--40. Turn all strings in a list to integers
stringsToIntegers :: [String] -> [Int]
stringsToIntegers [] = []
stringsToIntegers (x:xs) = read x : stringsToIntegers xs

--41. Get the fibonacci nubmers with the corresponding indices
fibonaccis :: (Eq a, Num a, Num b) => [a] -> [b]
fibonaccis [] = []
fibonaccis (x:xs) = fib x 1 0 : fibonaccis xs
                    where fib i val prev | i == 0 = prev
                                         | i == 1 = val
                                         | otherwise = fib (i-1) (val+prev) val

--42. Take a function and apply it to all elements of a list
applyToAll :: (a -> b) -> [a] -> [b]
applyToAll _ [] = []
applyToAll f (x:xs) = f x : applyToAll f xs

--43. Rewrite all of the above functions to use applyToAll

--44. Get all odd numbers in a list
odds :: (Integral a) => [a] -> [a]
odds [] = []
odds (x:xs) | mod x 2 == 1 = x : odds xs
            | otherwise = odds xs

--45. Return a function that filters all the numbers in a list divisible by 'n'
divisibles :: (Integral a) => a -> [a] -> [a]
divisibles _ [] = []
divisibles n (x:xs) | x `mod` n == 0 = x : divisibles n xs
                    | otherwise = divisibles n xs

--46. Take a predicate and filter a list
filterBy :: (a-> Bool) -> [a] -> [a]
filterBy _ [] = []
filterBy p (x:xs) | p x = x : filter p xs
                  | otherwise = filter p xs

--47. Rewrite all the functions using filterBy
-- Some other time :D

--48. Concatenate the lists
concat' :: [[a]] -> [a]
concat' (_:[]) = []
concat' (x:xs) = x ++ concat xs

--49. Reducing
--reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ acc [] = acc
reduce f acc (x:xs) = reduce f (f acc x) xs

--50. Reduce in the other direction
--reduce' :: (a -> b -> a) -> a -> [b] -> a
reduce' _ acc [] = acc
reduce' f acc (x:xs) = reduce' f (f x acc) xs

--51. Zip with a function
-- NOT WORKING
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
