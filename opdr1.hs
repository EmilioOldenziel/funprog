import Data.List

-- Helpfunction which gives the square of a integer.
isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

-- Checks if a given number is a prime.
isPrime :: Integer -> Bool
isPrime n
	| a == 0 = True
	| otherwise = False
	where a = sum [fromEnum (mod n x == 0) | x <- [2..(n-1)]]

-- Counts the amount of primes between 0 and a given upperbound.
cntPrime:: Integer -> Int
cntPrime n = sum[fromEnum (isPrime x) | x <- [1..n]]

-- Computes a list of pseudoprimes to a given upperbound.
oddPspTO :: Integer -> Integer -> [Integer]
oddPspTO a upb = [n | n <- [3,5..upb], a^(n-1) `mod` n == 1, not (isPrime n)]

-- Computes a^e % n
expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n 
	| e == 1 = a `mod` n
	| e `mod` 2 == 1 = (a * (expmod (a*a `mod` n) (e`div`2) n)) `mod` n
	| e `mod` 2 == 0 = (expmod (a*a `mod` n) (e`div`2) n) `mod` n

-- Improved version of oddPspTO due to using expmod.
oddPspTOI :: Integer -> Integer -> [Integer]
oddPspTOI a upb = [n | n <- [3,5..upb], expmod a (n-1) n == 1, not (isPrime n)]

-- Gives the prime factorisation of a given number.
factor :: Integer -> [Integer]
factor a = fac a 2 
	where 
	fac a i
		| a == i			= [i]
		| a `mod` i == 0	= [i] ++ (fac (a`div`i) i)
		| otherwise			= fac a (i+1)
			
-- Computes the  order of 2 in the group modulo p.
order2 :: Integer -> Integer -> Integer
order2 a p = head [e | e <- productset (factor (p-1)), expmod a e p == 1] 

-- Merges two ordered list to one ordered list
merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : (merge xs (y:ys))
  | x == y    = x : (merge xs ys)
  | otherwise = y : (merge (x:xs) ys)
  
-- Creates the productset of a list
productset :: [Integer] -> [Integer]
productset []     = [1]
productset (x:xs) = merge ps [x*n | n <- ps] 
  where ps = productset xs
  
-- Exercise 2
--------------------------------------------------

-- Checks the list of canidates if they are a pseudoprime.
oddPspTOII :: Integer -> Integer -> [Integer]
oddPspTOII a upb = [n | n <- candi a upb,  expmod a (n-1) n == 1, not (isPrime n)]

-- Makes the list of candidates that can be a pseudoprime. 
candi :: Integer -> Integer -> [Integer]
candi a upb = merge2 (candiList a upb)

-- Merges a list of ordered lists to one ordered list.
merge2 :: [[Integer]] -> [Integer]
merge2 [] = []
merge2(x:xs) = merge x (merge2 xs)

-- makes a list of ordered list with candates, where the primes are lower than the square of the upperbound.
candiList :: Integer -> Integer -> [[Integer]]
candiList a upb =  [candiP a p upb|p <- takeWhile(< isqrt(upb))primes]

-- makes a list of the candidates for a give prime.
candiP :: Integer -> Integer -> Integer -> [Integer]
candiP a p upb
	|odd (order2 a p) = [(((order2 a p) * p * 2) + p),(((order2 a p) * p * 4) + p) .. upb]
	|otherwise = [(((order2 a p) * p) + p),(((order2 a p) * p * 2) + p) .. upb]

-- Sieve of Eratosthenes, is a infinite list.
primes :: [Integer]
primes = sieve [ 3, 5..]
  where
  sieve (p:x) = p : sieve [ n | n <- x, n `mod` p > 0 ] 

-- A faster prime checker.
isPrime2 :: Integer -> Integer -> Bool
isPrime2 a x
	| b == 1 = True
	| otherwise = False
	where b = sum ([(toInteger (fromEnum(n == x))) | n <- ([3,5..x] \\ (oddPspTOII a x)), expmod a (n-1) n == 1])
	
-- Does not include 2 in the length due to making a list of odd numbers *[3,5 .. upb]*.
cntPrime2 :: Integer -> Integer -> Int
cntPrime2 a upb = length ([n | n <- ([3,5..upb] \\ (oddPspTOII a upb)), expmod a (n-1) n == 1])
