module Even3 (Even, (<+>), zero, two, evenValue) where

import Even2

data Even = Even (EvenPrime -> EvenPrime)

zero :: Even
zero =  Even id

two :: Even
two =  Even plus2'

(<+>) :: Even -> Even -> Even
(Even f) <+> (Even g) = Even (f . g)


evenValue :: Even -> Int
evenValue (Even f) = evenPrimeValue (f zero')

instance Show Even where
  show = show . evenValue
