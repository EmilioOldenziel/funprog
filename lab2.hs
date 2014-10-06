import Data.List
import Data.Function

-- Sieve of Eratosthenes, is a infinite list.
primes :: [Integer]
primes = 2:(sieve [ 3, 5..])
  where
  sieve (p:x) = p : sieve [ n | n <- x, n `mod` p > 0 ] 
  
 -- Computes a^e % n
expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n 
	| e == 1 = a `mod` n
	| e `mod` 2 == 1 = (a * (expmod (a*a `mod` n) (e`div`2) n)) `mod` n
	| e `mod` 2 == 0 = (expmod (a*a `mod` n) (e`div`2) n) `mod` n

-- Calculates the smallest multiplier of the list 1 to n
smallestMultiplier n =  foldr lcm 1 [2..n]

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

-- Checks if a number is a palindrome
isPalindromicNumber :: Integer -> Bool
isPalindromicNumber n = n == read(reverse(show n))
  
-- Helpfunction which gives the square of a integer.
isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromIntegral

select :: Integer -> Integer -> [Integer] -> [Integer]
select m n xs = takeWhile (<= n) (dropWhile (<m) xs) 

numberOfPalindromicComposites upb = length[x | x  <- (createPalins upb), length (factor3 x) == 2,  not(x==10) ]

createPalins:: Integer -> [Integer]
createPalins n = concat [(createPalinsN x) | x <- [1..n]]

createPalinsN :: Integer -> [Integer]
createPalinsN n  = [read(x)| x <- palindrome n ['0'..'9']]

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

mults :: Integer -> [Integer]
mults n = [n, 2*n ..]

multiples :: [Integer] ->[Integer]
multiples xs = foldr merge [] (map mults xs)

multSum :: Integer -> [Integer] -> Integer
multSum n xs = sum (takeWhile (< n) (multiples xs))

powers :: Integer -> [Integer]
powers a = map (\x -> a^x) [2..]

distinctPowers :: Integer -> Int -> Int
distinctPowers m n = length( foldr merge [] (map (take (n - 1)) (map powers [2..m])))

--Converts integer to list of the numbers in the integer
digits :: Integer -> [Integer]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

takeLast :: Int -> [Integer] -> [Integer]
takeLast n xs = reverse( take (n) (reverse xs))

lastDigits :: Integer -> Int-> [Integer]
lastDigits m n  = takeLast n (digits (sum[expmod x x (10^n) | x <- [1..m] ]))

-- ex 6
factorial :: Integer -> Integer
factorial n = foldr (*) 1 [1..n]

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

-- ex 7

--getComma :: []
--getComma n = (0,:xs)

division :: (Enum b, Fractional b) => b -> b -> [(b, b)]
division m n = zip [m..n] [(1 / x)|x <- [m..n]]


