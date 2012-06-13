try' :: Int -> Int
try' 0 = 0 
try' x
     | divi' x 20 == True = x 
     | otherwise = try' $ x + 1

divi' :: Int -> Int -> Bool  
divi' x 1 = True  
divi' x y
    | x `mod` y == 0  = divi' x (y-1)  
    | otherwise = False
