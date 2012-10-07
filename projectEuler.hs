import Data.List
import Data.Maybe

sumThreeFive :: Integer -> Integer
sumThreeFive x = sum(filter p [1..x])  
    where p x = x `mod` 3 == 0 || x `mod` 5 == 0 

fibo :: [Integer] -> [Integer]
fibo base = base ++ [(last base + last(init base))]

fiboLim :: Integer -> [Integer] -> [Integer]
fiboLim x xs = 
	if (last (fibo xs)) < x then
		fiboLim x (fibo xs)
	else	
		xs			

divisors n = filter ((== 0) . (n `mod`) ) [2 .. ceiling $ fromIntegral n/2 ]

factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ factor $ div n prime

maxPrimeFactor x = maximum( factor x ) 

isPalin x = (show x) == (reverse $ show x)

threeLen :: Integer -> Bool
threeLen x = length ( show x ) == 3

threeDiv :: Integer -> Integer
threeDiv x
	| find( \n -> threeLen n == True && threeLen ( x `div` n ) == True ) ( reverse (divisors x) ) == Nothing = 0
	| otherwise = fromJust( find( \n -> threeLen n == True && threeLen ( x `div` n ) == True ) ( reverse (divisors x) ) )

threeTest :: Integer -> Integer
threeTest x
	| isPalin x = threeDiv x
	| otherwise = 0

largestThreePalin :: [Integer] -> Integer
largestThreePalin xs = fromJust( find( \x -> threeTest x /= 0 ) xs )

step :: Integer -> Integer
step x = x + 1

isDiv :: (Integer, Integer) -> Bool
isDiv (x, y) = x `mod` y == 0

smallNum :: Integer -> Integer
smallNum x =
	if dropWhile( (==0) . ( x `mod` ) )[1..20] /= [] then	
		smallNum (x+1)
	else 
		x


