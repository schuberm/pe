import Data.List
import Data.Ord

test' xs = [ a*b*c | c <- [1..xs], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == xs]  
