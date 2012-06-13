module Main where
import Data.List

main = do
	print (sort (mid [0..9]) !! 1000000)
	
mid :: [Integer] -> [Integer]
mid xs = [ convert' x | x <- permutations xs]

convert' :: [Integer] -> Integer 
convert' xs = foldr (\x acc -> acc + x*10^(head (elemIndices x  (reverse xs)))) 0 xs
