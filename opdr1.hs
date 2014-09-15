isPrime :: Integer -> Bool
isPrime n
	| a == 0 = True
	| otherwise = False
	where a = sum [fromEnum (mod n x == 0) | x <- [2..(n-1)]]

cntPrime:: Integer -> Int
cntPrime n = sum[fromEnum (isPrime x) | x <- [1..n]]

oddPspTO :: Integer -> Integer -> [Integer]
oddPspTO a upb = [n | n <- [3,5..upb], a^(n-1) `mod` n == 1, not (isPrime n)]

expmod :: Integer -> Integer -> Integer -> Integer
expmod a e n 
	| e == 1 = a `mod` n
	| e `mod` 2 == 1 = (a * (expmod (a*a) (e`div`2) n)) `mod` n
	| e `mod` 2 == 0 = (expmod (a*a) (e`div`2) n) `mod` n

oddPspTOI :: Integer -> Integer -> [Integer]
oddPspTOI a upb = [n | n <- [3,5..upb], expmod a (n-1) n == 1, not (isPrime n)]

order :: Integer -> Integer -> Integer
order a p = ord a (a `mod` p) 1 p
	where ord a e k p = if e == 1 then k else ord a (a*e `mod` p) (k+1) p 

factor :: Integer -> [Integer]
factor a 
	| xs = []
	| fac a 2
	where fac a i = if i == a then xs ++ [a] else ( if a%i == 0 then a `div` i && xs ++ [i] else fac a (i+1))

order2 :: Integer -> Integer -> Integer
order2 a p =  
