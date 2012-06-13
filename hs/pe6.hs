diff xs = squaresum xs - sumsquare xs

sumsquare :: Integer -> Integer
sumsquare xs = sum [x^2 | x <- [1..xs]]

squaresum :: Integer -> Integer
squaresum xs = (sum [x| x <- [1..xs]])^2

