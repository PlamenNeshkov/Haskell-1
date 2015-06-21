double :: (Num a) => [a] -> [a]
double [] = []
double (x:xs) = x * 2 : double(xs)
