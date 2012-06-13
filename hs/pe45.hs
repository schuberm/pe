import Data.Set

solutions = [ a | a <- tri, ispent a && ishex a]

istry = (`member` fromList  tri)
ispent = (`member` fromList  pent)
ishex =  (`member` fromList  hex)
tri = [(n * (n+1)) `div` 2 | n <- [1..1000000]]
pent = [(n * (3*n-1)) `div` 2 | n <- [1..1000000]]
hex = [(n * (2*n-1)) | n <- [1..1000000]]
