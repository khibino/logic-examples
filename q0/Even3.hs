module Even3 (Even, (<+>), zero, two, evenValue) where

import Even2

data Even = Even (EvenPrime -> EvenPrime)

evenValue :: Even -> Int
evenValue (Even f) = evenPrimeValue (f zero')

(<+>) :: Even -> Even -> Even
(Even f) <+> (Even g) = Even (f . g)

zero :: Even
zero =  Even id

two :: Even
two =  Even plus2'


instance Show Even where
  show = show . evenValue
