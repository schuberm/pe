import Data.Char

check' =  reverse (take 10 (reverse (map digitToInt $ show (sum square))))
square = [x^x |x <- [1..1000]]
