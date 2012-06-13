mul35 :: [Int]->Int
mul35 xs = sum [ x | x <- xs, x `mod` 3 == 0 || x `mod` 5 == 0] 
