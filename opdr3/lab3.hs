import Data.List
import Data.Function
import Data.Char

import Compare
import Expression
import Types
import Valuations



-- Part 02 Exercise 01:
-- toComparison

toComparison :: String -> Comparison
toComparison str = Cmp (checkOperator (getCom str)) (toExpr(eOne str)) (toExpr(eTwo str))

eOne :: [Char] -> [Char]
eOne str = takeWhile (not.(\x -> elem x "<>=#")) str

eTwo ::[Char] -> [Char]
eTwo str = reverse (takeWhile (not.(\x -> elem x "<>=#")) (reverse str))

--takes the comparator sign from a expression string
getCom:: [Char] -> [Char]
getCom str = (reverse (dropWhile (not.(\x -> elem x "<>=#")) (reverse (dropWhile (not.(\x -> elem x "<>=#")) str))))

checkOperator:: [Char] -> Relop
checkOperator "<" = LessThan
checkOperator "<=" = LessEqual
checkOperator "=" = Equal
checkOperator ">" = Greater
checkOperator ">=" = GreaterEqual
checkOperator "#" = NotEqual


evalCmp :: Comparison -> Valuation -> Bool
evalCmp (Cmp LessThan eOne eTwo) xs 	= (evalExpr eOne xs) < 	(evalExpr eTwo xs)
evalCmp (Cmp LessEqual eOne eTwo) xs 	= (evalExpr eOne xs) <= (evalExpr eTwo xs)
evalCmp (Cmp Equal eOne eTwo) xs 		= (evalExpr eOne xs) == (evalExpr eTwo xs)
evalCmp (Cmp Greater eOne eTwo) xs 		= (evalExpr eOne xs) > 	(evalExpr eTwo xs)
evalCmp (Cmp GreaterEqual eOne eTwo) xs = (evalExpr eOne xs) >= (evalExpr eTwo xs)
evalCmp (Cmp NotEqual eOne eTwo) xs 	= (evalExpr eOne xs) /= (evalExpr eTwo xs)

