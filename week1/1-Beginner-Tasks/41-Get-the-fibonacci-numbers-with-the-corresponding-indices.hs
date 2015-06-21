fibonaccis :: (Eq a, Num a, Num b) => [a] -> [b] 
fibonaccis [] = []
fibonaccis (x:xs) = fib x 1 0 : fibonaccis xs
                    where fib i val prev | i == 0 = prev
                                         | i == 1 = val
                                         | otherwise = fib (i-1) (val+prev) val
