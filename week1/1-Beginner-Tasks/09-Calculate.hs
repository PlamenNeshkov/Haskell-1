calculate :: Char -> Float -> Float -> Float
calculate op a b = | op == '+' = a + b else
				   | op == '-' = a - b else
				   | op == '*' =  a * b else
				   | op == '/' =  a / b else
				   | otherwise = error "Unknown operator"
