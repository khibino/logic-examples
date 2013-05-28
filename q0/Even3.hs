module Even3 (Even, (<+>), zero, two, evenValue) where

import Even2

data Even = Even (EvenPrime -> EvenPrime)

(<+>) :: Even -> Even -> Even
(Even f) <+> (Even g) = Even (f . g)

zero :: Even
zero =  Even id

two :: Even
two =  Even plus2'


evenValue :: Even -> Int
evenValue (Even f) = evenPrimeValue (f zero')

instance Show Even where
  show = show . evenValue
