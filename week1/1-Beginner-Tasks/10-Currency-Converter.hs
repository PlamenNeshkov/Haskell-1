convert :: String -> String -> Float -> Float
convert "bgn" "usd" value = value * 0.58;
convert "bgn" "eur" value = value * 0.51;
convert "usd" "bgn" value = value * 1.72;
convert "usd" "eur" value = value * 0.88;
convert "eur" "usd" value = value * 1.14;
convert "eur" "bgn" value = value * 1.96;
convert _     _     value = error "Invalid currencies"
