unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([],[])
unzip' ((x, y):zs) = (x : fst pair, y : snd pair)
                      where pair = unzip' zs
