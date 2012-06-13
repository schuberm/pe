facsum :: Integer -> Integer
facsum xs = sum [factorial x | x <- [0..xs]]

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1) 
