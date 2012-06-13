module Main where
import Data.List

main = do
	print (sum [convert' x | x<-d2d3d4 [0..9]])

d2d3d4 :: [Integer] -> [[Integer]]
d2d3d4 xs = [x | x <- permutations xs , (convert' [x!!1,x!!2,x!!3]) `mod` 2 == 0 && (convert' [x!!2,x!!3,x!!4]) `mod` 3 == 0 && (convert' [x!!3,x!!4,x!!5]) `mod` 5 == 0 && (convert' [x!!4,x!!5,x!!6]) `mod` 7 == 0 && (convert' [x!!5,x!!6,x!!7]) `mod` 11 == 0 && (convert' [x!!6,x!!7,x!!8]) `mod` 13== 0 && (convert' [x!!7,x!!8,x!!9]) `mod` 17== 0 ]

convert' :: [Integer] -> Integer 
convert' xs = foldr (\x acc -> acc + x*10^(head (elemIndices x  (reverse xs)))) 0 xs
