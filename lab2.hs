import Data.List
import Data.Function


-- Helpfunctions in alphabetic order

-- Creates palindromes
createPalins:: Integer -> [Integer]
createPalins n = concat [(createPalinsN x) | x <- [1..n]]

-- Creates palindromes of a certain length n
createPalinsN :: Integer -> [Integer]
createPalinsN n  = [read(x)| x <- palindrome n ['0'..'9']]

--Converts integer to list of the numbers in the integer
digits :: Integer -> [Integer]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- Drops all elements of a list after the element that is is equal to n,
-- including that element itself
dropAt:: [Integer] -> Integer -> [Integer]
dropAt (x:xs) ys n
		| x == n = ys
		| otherwise = dropAt xs (x:ys) n

 -- Computes a^e % n
expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n 
	| e == 1 = a `mod` n
	| e `mod` 2 == 1 = (a * (expmod (a*a `mod` n) (e`div`2) n)) `mod` n
	| e `mod` 2 == 0 = (expmod (a*a `mod` n) (e`div`2) n) `mod` n
	
-- Gives the prime factorisation of a given number.
factor :: Integer -> [Integer]
factor n = fac n primes 
	where 
  	 fac n (p:primes)
  	    | n == 1            = []
		| n `mod` p == 0	= [p] ++ (fac (n `div` p) (p:primes))
		| p*p > n           = [n]
		| otherwise			= fac n primes

-- Gives at most 3 primes factors of a given number.
factor3 :: Integer -> [Integer]
factor3 n = take 3 (factor n)

-- Gives the factorial of n
factorial :: Integer -> Integer
factorial n = foldr (*) 1 [1..n]

-- Checks if a number is a palindrome
isPalindromicNumber :: Integer -> Bool
isPalindromicNumber n = n == read(reverse(show n))

-- Gives the square of a integer.
isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

-- Gives a list with the repetition using long division
longDiv :: Integer -> Integer -> [Integer] -> [Integer]
longDiv p q xs
	| (p`mod`q) `elem` xs = dropAt xs [] (p `mod` q)
	| p `mod` q == 0 = []
	| otherwise = longDiv ((p`mod`q)*10) q ((p`mod`q) : xs)

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

-- Takes a finite list of integers and creates an
-- infinite list of its multiples
multiples :: [Integer] ->[Integer]
multiples xs = foldr merge [] (map mults xs)

-- Creates an infinite list of multiples of n
mults :: Integer -> [Integer]
mults n = [n, 2*n ..]

-- Creates palindromes of length n
palindrome :: (Integral a) => a -> [Char] -> [String]
palindrome n al =  concat  $  map (pal n) al
	where 
   pal :: (Integral a)=> a -> Char -> [String]
   pal n x 
		| n > 2 =  map (surround x) (palindrome (n-2) al)
		| n > 1 = [[x,x]]
		| otherwise = [[x]]
		where 
			surround :: Char -> String -> String
			surround lt str = [lt] ++ str ++ [lt]

-- Creates an infinite list of the powers of a
powers :: Integer -> [Integer]
powers a = map (\x -> a^x) [2..]

-- Sieve of Eratosthenes, is a infinite list.
primes :: [Integer]
primes = 2:(sieve [ 3, 5..])
  where
  sieve (p:x) = p : sieve [ n | n <- x, n `mod` p > 0 ] 
  
-- Gives a list with the repetition using long division
rep :: Integer -> Int
rep q =  length (longDiv 1 q [])

-- Applies takeWhile and dropWhile to a list
select :: Integer -> Integer -> [Integer] -> [Integer]
select m n xs = takeWhile (<= n) (dropWhile (<m) xs) 

-- Takes the last n elements of a list xs
takeLast :: Int -> [Integer] -> [Integer]
takeLast n xs = reverse( take (n) (reverse xs))

-- Excercises

--  Exercise 1: Calculates the smallest multiplier of the list 1 to n
smallestMultiplier :: Integer -> Integer
smallestMultiplier n =  foldr lcm 1 [2..n]

-- Exercise 2: sums up all integers of a list till the nth element of
-- the list
multSum :: Integer -> [Integer] -> Integer
multSum n xs = sum (takeWhile (< n) (multiples xs))

-- Exercise 3: produces the number of distinct terms in the sequence generated by a^b
-- for 2 <= a <= m and 2 <= b <= n.
distinctPowers :: Integer -> Int -> Int
distinctPowers m n = length( foldr merge [] (map (take (n - 1)) (map powers [2..m])))

-- Exercise 4: Gives the number of palindromic composites under a upperbound 
numberOfPalindromicComposites :: Integer -> Int
numberOfPalindromicComposites upb = length[x | x  <- (createPalins upb), length (factor3 x) == 2,  not(x==10) ]

-- Exercise 5: Gives the last n digits of the sum of all digits of the list
-- 1^1, 2^2, .., m^m
lastDigits :: Integer -> Int-> [Integer]
lastDigits m n  = takeLast n (digits (sum[expmod x x (10^n) | x <- [1..m] ]))

-- Exercise 6:  All the functions described in ex. 6.
-- function 'f' is named 'function'
function :: Integer -> Integer
function n = sum (map factorial (digits n))

sFunction :: Integer -> Integer
sFunction n = sum(digits(function n))

gFunction :: Integer -> Integer
gFunction i = head [x | x <- [1..], sFunction x == i]

sGFunction :: Integer -> Integer
sGFunction i = sum(digits(gFunction i))

sumsg :: Integer -> Integer
sumsg n = sum (map sGFunction [1..n])

-- Exercise 7: Gives the number n for which 1/n has the longest
-- repetitive reciprocal of the list [m..n]
division :: Integer -> Integer -> Integer
division m n = snd (maximum (zip (map rep [m..n]) [m..n])) 




