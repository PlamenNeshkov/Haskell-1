isFibonacciSequence :: (Eq a, Num a) => [a] -> Bool
isFibonacciSequence (x:y:[]) = True
isFibonacciSequence (x:y:z:zs) = if z == y + x
                                  then isFibonacciSequence (y:z:zs)
                                  else False
