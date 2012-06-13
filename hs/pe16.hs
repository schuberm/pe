import Data.Char

check' xs =  sum (map digitToInt $ show (xs^1000))
