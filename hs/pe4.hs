import Data.Char

check' :: [Int] -> [[Int]]
check' xs = [[ y*x | y <- xs, (map digitToInt $ show (y*x)) == (reverse ( map digitToInt $ show (y*x)))] | x <- xs ]
