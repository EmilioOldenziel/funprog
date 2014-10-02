import Data.List

-- Write a Haskell function smallestMultiple n that returns the smallest number than can be divided
-- by each of the numbers from 1 to n without any remainder. For example,
-- smallestMultiple 10 should yield the answer 2520. Your solution must make use of the standard Haskell function foldr,
-- and should compute smallestMultiple 10000 within one second.

--smallestMultiplier :: (a -> b -> b) -> b -> [a] -> [[a]]



smallestMultiplier n =  foldr lcm 1 [2..n]

-- smallestMultiplier n =  foldr (*) 1 (foldr merge [] (map factor [2..n]))

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
  
-- Merges a list of ordered lists to one ordered list.
merge2 :: [[Integer]] -> [Integer]
merge2 [] = []
merge2(x:xs) = merge x (merge2 xs)  

-- We call a positive integer n a palindromic number if reversing its digits yields the same number. For
-- example, 123321 is a palindromic number, while the number 123 is clearly not palindromic. Write a
-- Haskell function numberOfPalindromicComposites that takes as its input a positive integer
-- n, and produces as its output the number of palindromic numbers x which have the form
-- x=p*q<n, where p and q are prime numbers. For example, numberOfPalindromicComposites 100
-- should yield 7 (which is length [4,6,9,22,33,55,77].
-- The time to compute palindromicComposites (10^8) using ghci should not exceed two seconds.

isPalindromicNumber :: Integer -> Bool
isPalindromicNumber n = n == read(reverse(show n))

palindromes :: [Integer]
palindromes = genPalin []
	where genPalin xs =
		[(x:xs)| x<-[0..9]]
		x:(reverse xs)
	
-- Sieve of Eratosthenes, is a infinite list.
primes :: [Integer]
primes = 2:(sieve [ 3, 5..])
  where
  sieve (p:x) = p : sieve [ n | n <- x, n `mod` p > 0 ] 
  
-- Helpfunction which gives the square of a integer.
isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

select :: Integer -> Integer -> [Integer] -> [Integer]
select m n xs = takeWhile (<= n) (dropWhile (<m) xs) 

numberOfPalindromicComposites n = length[x*y| x<-xs, y<-select x (n `div` x) primes, isPalindromicNumber(x*y)]
	where xs = select 2 (isqrt n) primes 
