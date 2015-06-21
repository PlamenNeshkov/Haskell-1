stringsToIntegers :: [String] -> [Int]
stringsToIntegers [] = []
stringsToIntegers (x:xs) = read x : stringsToIntegers xs
