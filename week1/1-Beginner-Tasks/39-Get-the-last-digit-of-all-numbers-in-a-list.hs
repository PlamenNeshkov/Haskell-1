lastDigits :: (Integral a) => [a] -> [a]
lastDigits [] = []
lastDigits (x:xs) = if x > 9 then (mod x 10):lastDigits xs else x:lastDigits xs
