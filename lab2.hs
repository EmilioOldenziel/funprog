smallestMultiple :: Integer -> Integer
smallestMultiple n = sum[(n `div` x)  | x <- [1 .. n]]
