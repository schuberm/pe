isprime :: Integer -> [Integer]
isprime xs = [x | x <- [2..xs], xs `mod` x == 0]
