module Valuations where 
import Types
type Valuation = [(Name, Integer)]

-- Part 01 Exercise 03: 
-- Geeft alle valuaties van de namen	
valuations :: [(Name, Domain)] -> [Valuation]
valuations [] = [[]]
valuations ((_,[]):xs) = [] 
valuations ((n, d:ds) : xs) = [(n, d):p | p <- valuations xs] ++ (valuations ((n, ds):xs))
