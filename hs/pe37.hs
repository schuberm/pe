--module Main where
--import Data.List
import Prelude hiding (map) 
import Data.Array.Unboxed
import Data.Char
import Data.Set

--list' = map .digitToInt. show

--digits :: Integer -> [Int]
--digits n = map (\x -> read [x] :: Int) (show n)

digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10)

isPrime m = (`member` fromList (primesToA m))

primesToA m = sieve 3 (array (3,m) [(i,odd i) | i<-[3..m]]
                        :: UArray Int Bool)
  where
    sieve p a 
      | p*p > m   = 2 : [i | (i,True) <- assocs a]
      | a!p       = sieve (p+2) $ a//[(i,False) | i <- [p*p, p*p+2*p..m]]
      | otherwise = sieve (p+2) a
