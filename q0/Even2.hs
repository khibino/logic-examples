module Even2 (EvenPrime, zero', plus2', evenPrimeValue) where

data EvenPrime = Even' Int deriving Show

zero' :: EvenPrime
zero' =  Even' 0

plus2' :: EvenPrime -> EvenPrime
plus2' (Even' i) = Even' (i + 2)

evenPrimeValue :: EvenPrime -> Int
evenPrimeValue (Even' i) = i