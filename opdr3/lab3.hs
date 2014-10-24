import Data.List
import Data.Function
import Data.Char

import Compare
import Expression
import Types
import Valuations

-- Part 01 Exercise 01: 
-- Pretty print expressions
par :: String -> String
par s = "(" ++ s ++ ")"

instance Show Expr where
	show (Var x) = x
	show (Val x) = show x
	show (p :+: q) = par(show p ++ " + " ++ show q)
	show (p :-: q) = par(show p ++ " - " ++ show q)
	show (p :*: q) = par(show p ++ " * " ++ show q)
	show (p :/: q) = par(show p ++ " / " ++ show q)
	show (p :%: q) = par(show p ++ " % " ++ show q)

-- Part 01 Exercise 02:
-- Sorted list of variables
vars (Var x) = [x]
vars (Val x) = [show x]
vars (p :+: q) = nub (vars p ++ vars q)
vars (p :-: q) = nub (vars p ++ vars q)
vars (p :*: q) = nub (vars p ++ vars q)
vars (p :/: q) = nub (vars p ++ vars q)
vars (p :%: q) = nub (vars p ++ vars q)

-- Part 01 Exercise 04:
-- Evaluating Exprs
evalExpr :: Expr -> Valuation -> Integer
evalExpr e xs = read (show (eval e xs))::Integer
eval (Var x) xs= head [snd n | n <- xs, (fst n) == x] 
eval (Val x) xs= x
eval (p :+: q) xs= (eval p xs) + (eval q xs)
eval (p :-: q) xs= (eval p xs) - (eval q xs)
eval (p :*: q) xs= (eval p xs) * (eval q xs)
eval (p :/: q) xs= div (eval p xs) (eval q xs)
eval (p :%: q) xs= mod (eval p xs) (eval q xs)

-- Part 01 Exercise 05:
-- Parsing expressions: String to Expr
toExpr :: String -> Expr
toExpr str = fst (parser str)

-- Part 02 Exercise 01:
-- toComparison

--toComparison :: String -> Comparison
--toComparison str = Cmp (checkOperator (eTwo (eOne str))) (toExpr(eOne str)) (toExpr(eTwo str))

eOne str = takeWhile (\x -> (isInfixOf x "<=>#")) str

eTwo str = reverse (takeWhile (\x -> (isInfixOf x "<=>#")) (reverse str))

checkOperator "<" = "LessThan"
checkOperator "<=" = "LessEqual"
checkOperator "=" = "Equal"
checkOperator ">" = "Greater"
checkOperator ">=" = "GreaterEqual"
checkOperator "#" = "NotEqual"
{--
com :: String -> [String]
com [] = []
com (x:xs)
	| elem x "*+/-%()" = [x] : (com xs)	
	| elem x " " = com xs
	| isAlpha x = (x:takeWhile isAlpha xs) : com (dropWhile isAlpha xs)
	| isDigit x = (x:takeWhile isDigit xs) : com (dropWhile isDigit xs)
	| elem x ["<","=",">"," #" ] = (x:takeWhile (elem x ["<","=",">"," #" ]) ) : com (dropWhile isDigit xs)
	| otherwise = error "Syntax Error: invalid character in input"

--}
