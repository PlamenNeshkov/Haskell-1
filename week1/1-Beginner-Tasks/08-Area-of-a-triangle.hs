area :: (Float, Float, Float) -> Float
area (a, b, c) = sqrt (p
	* (p - a)
	* (p - b)
	* (p - c))
	where p = (a + b + c) / 2
