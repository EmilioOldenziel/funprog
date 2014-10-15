import Data.List
import Data.Function
import Data.Char
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
evalExpr e xs = and (valid e xs)

valid :: Expr -> Valuation -> [Bool]
valid (Val x) = True
valid (Var x) = va(x, xs)
valid (p :+: q) = valid p && valid q
valid (p :-: q) = valid p && valid q
valid (p :*: q) = valid p && valid q
valid (p :/: q) = valid p && valid q
valid (p :%: q) = valid p && valid q
 where va x xs
	| [] == [(n, _):p | p<-xs, x == n] = False
	| otherwise = True
	
-- Part 01 Exercise 05:
-- Parsing expressions: String to Expr

--tokenize :: [Char] -> [[Char]]
tokenize [] = []
tokenize (x:xs)
	| elem x "*+/-%" = [x] : (tokenize xs)	
	| elem x " " = tokenize xs
	| isAlpha x = (x:takeWhile isAlpha xs) : tokenize (dropWhile isAlpha xs)
	| isDigit x = (x:takeWhile isDigit xs) : tokenize (dropWhile isDigit xs)
	| otherwise = error "Syntax Error: invalid character in input"
	
expres :: [[Char]] -> String
expres [] = []
expres (x:xs)
	| isDigit x = (read x::Integer): ) : toExpr xs
	| isAlpha x = (read x::Char): ) : toExpr xs
	| elem x "+/-%" = ( : (read x::thingie) : toExpr xs

--toExpr :: String -> Expr
toExpr e = expres (tokenize e)


