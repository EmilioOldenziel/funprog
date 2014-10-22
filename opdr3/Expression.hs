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
