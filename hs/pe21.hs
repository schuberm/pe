sum' :: Integer -> Integer
sum' xs = sum [x | x <- [1..xs], amicable x == True]

amicable :: Integer -> Bool
amicable xs = if xs == sum (propdiv (sum (propdiv xs))) then True else False

propdiv :: Integer -> [Integer]
propdiv xs = [x | x <- [1..(xs-1)], xs `mod` x == 0]

