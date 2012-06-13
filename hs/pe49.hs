module Main where
import Data.List
import Data.Array.Unboxed
import Data.Char

main = do
	print (check' [3330])

check' :: [Int] -> [[Int]]
check' xs = [[ y | y <- primesToA 10000, (y+x) `elem` primesToA 10000 && (y+x+x) `elem` primesToA 10000 && sort( map digitToInt $ show (y+x)) ==  sort (map digitToInt $ show y) && sort( map digitToInt $ show (y+x+x)) ==  sort (map digitToInt $ show y)] | x <- xs ]

primesToA m = sieve 3 (array (3,m) [(i,odd i) | i<-[3..m]]
                        :: UArray Int Bool)
  where
    sieve p a 
      | p*p > m   = 2 : [i | (i,True) <- assocs a]
      | a!p       = sieve (p+2) $ a//[(i,False) | i <- [p*p, p*p+2*p..m]]
      | otherwise = sieve (p+2) a
