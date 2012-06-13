import Data.Set

{-# LANGUAGE ParallelListComp #-}

pent = 1 : [x*(3*x-1)/2 | x <- [2..10000]]
isPenta = (`member` fromList  pent)
pentdiff = [[ (a,b,a-b) | a <- pent, isPenta (a-b) && isPenta (a+b) ]| b <- pent]


