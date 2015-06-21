pyths :: Int -> Int -> [(Int, Int, Int)]
pyths from to = [(a, b, c) | a <- [from..to], b <- [from..to], c <- [from..to],
                a < b, b < c, (a^2 + b^2 == c^2)]
