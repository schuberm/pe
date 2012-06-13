pe3 :: Integer -> [Integer]
pe3 xs = [x | x <- propDiv xs , isPrime x == True]

isPrime :: Integer -> Bool
isPrime xs = if [x | x <- [2..xs], xs `mod` x == 0]==[] then True else False

propDiv :: Integer -> [Integer]
propDiv xs = [x | x <- [1..(xs-1)], xs `mod` x == 0]

