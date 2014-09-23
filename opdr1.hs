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
factor a = fac a 2 
	where 
	fac a i
		| a == i			= [i]
		| a `mod` i == 0	= [i] ++ (fac (a`div`i) i)
		| otherwise			= fac a (i+1)
			

order2 :: Integer -> Integer -> Integer
order2 a p = head [e | e <- productset (factor (p-1)), expmod a e p == 1] 

merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : (merge xs (y:ys))
  | x == y    = x : (merge xs ys)
  | otherwise = y : (merge (x:xs) ys)
  

productset :: [Integer] -> [Integer]
productset []     = [1]
productset (x:xs) = merge ps [x*n | n <- ps] 
  where ps = productset xs

--oddPspTOII :: Integer -> Integer -> [Integer]
--oddPspTOII a upb = [n | n <- candi a upb, a^(n-1) `mod` n == 1, not (isPrime n)]

candi :: Integer -> Integer -> [Integer]
candi a upb = [merge(x:xs)|xs <- candiList a upb]

candiList :: Integer -> Integer -> [[Integer]]
candiList a upb =  merge[candiP a p upb|p <- takeWhile(<upb)primes]

candiP :: Integer -> Integer -> Integer -> [Integer]
candiP a p upb = [(((order2 a p) * p) + p),(((order2 a p) * p * 2) + p) .. upb]

primes :: [Integer]
primes = sieve [ 3, 5..]
  where
  sieve (p:x) = p : sieve [ n | n <- x, n `mod` p > 0 ] 

--main = length ( oddPspTO 2 2^16 )
