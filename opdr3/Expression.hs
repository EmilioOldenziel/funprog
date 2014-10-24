module Expression where
import Data.List
import Data.Function
import Data.Char
import Types
import Valuations

data Expr =
			Val Integer
		| Var Name
		| Expr :+: Expr
		| Expr :-: Expr
		| Expr :*: Expr
		| Expr :/: Expr
		| Expr :%: Expr
		

-- Part 01 Exercise 06:
-- Wrap it all up

instance Show Expr where
	show (Var x) = x
	show (Val x) = show x
	show (p :+: q) = par(show p ++ " + " ++ show q)
	show (p :-: q) = par(show p ++ " - " ++ show q)
	show (p :*: q) = par(show p ++ " * " ++ show q)
	show (p :/: q) = par(show p ++ " / " ++ show q)
	show (p :%: q) = par(show p ++ " % " ++ show q)

-- Part 01 Exercise 01: 
-- Pretty print expressions
par :: String -> String
par s = "(" ++ s ++ ")"



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
parseF :: [String] -> (Expr,[String])
parseF [] = error "Parse error...abort"
parseF (tok:tokens)
	| tok == "("		   = (expr, tail rest)
	| isAlpha	(head tok) = (Var tok, tokens)
	| isDigit	(head tok) = (Val (read tok), tokens)
	| otherwise 		 = error("Syntax Error: " ++ tok)
	where
		(expr, rest) = parseE tokens
		
parseT' :: Expr -> [String] -> (Expr,[String])
parseT' accepted ("*":tokens) = parseT' (accepted :*: term) rest
	where (term, rest) = parseF tokens
parseT' accepted ("/":tokens) = parseT' (accepted :/: term) rest
    where (term, rest) = parseF tokens
parseT' accepted ("%":tokens) = parseT' (accepted :%: term) rest
    where (term, rest) = parseF tokens
parseT' accepted tokens =  (accepted, tokens)

parseT :: [String] -> (Expr,[String])
parseT tokens = parseT' acc rest
  where (acc, rest) = parseF tokens

parseE' :: Expr -> [String] -> (Expr,[String])
parseE' accepted ("+":tokens) = parseE' (accepted :+: term) rest
	where (term, rest) = parseT tokens
parseE' accepted ("-":tokens) = parseE' (accepted :-: term) rest
	where (term, rest) = parseT tokens
parseE' accepted tokens = (accepted, tokens)

parseE :: [String] -> (Expr,[String])
parseE tokens = parseE' acc rest
  where (acc, rest) = parseT tokens
  
tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs)
	| elem x "*+/-%()" = [x] : (tokenize xs)	
	| elem x " " = tokenize xs
	| isAlpha x = (x:takeWhile isAlpha xs) : tokenize (dropWhile isAlpha xs)
	| isDigit x = (x:takeWhile isDigit xs) : tokenize (dropWhile isDigit xs)
	| otherwise = error "Syntax Error: invalid character in input"

parser :: String -> (Expr,[String])
parser str = parseE (tokenize str)
