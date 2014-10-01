import Data.List

-- Write a Haskell function smallestMultiple n that returns the smallest number than can be divided
-- by each of the numbers from 1 to n without any remainder. For example,
-- smallestMultiple 10 should yield the answer 2520. Your solution must make use of the standard Haskell function foldr,
-- and should compute smallestMultiple 10000 within one second.

--smallestMultiplier :: (a -> b -> b) -> b -> [a] -> [[a]]

-- foldr `div` [n, 2*n ..]

smallestMultiplier n =  foldr (*) 1 (foldr merge [] (map factor [2..n]))

-- Gives the prime factorisation of a given number.
factor :: Integer -> [Integer]
factor a = fac a 2 
	where 
	fac a i
		| a == i			= [i]
		| a `mod` i == 0	= [i] ++ (fac (a`div`i) i)
		| otherwise			= fac a (i+1)

-- Merges two ordered list to one ordered list
merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : (merge xs (y:ys))
  | x == y    = x : (merge xs ys)
  | otherwise = y : (merge (x:xs) ys)
