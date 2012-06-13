
lc = [ chain x | x <-[1..999999]]

chain :: Int -> [Int] 
chain n     | n == 0       = error "What are you on about?"
            | n == 1       = []
            | rem n 2 == 0 = (n `div` 2) : chain (n `div` 2)
            | otherwise    = (3 * n + 1) : chain (3 * n + 1)
