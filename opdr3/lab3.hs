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
toComparison :: String -> Comparison
toComparison (p < q) = Cmp LessThan p q
toComparison (p <= q) = Cmp LessEqual p q
toComparison (p = q) = Cmp Equal p q
toComparison (p > q) = Cmp GreaterThan p q
toComparison (p >= q) = Cmp GreaterThan p q
toComparison (p # q) = Cmp NotEqual p q
