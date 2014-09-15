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
