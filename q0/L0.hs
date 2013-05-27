module L0 where

import Data.List (permutations)

data Person = Tonchiki
            | Chinpei
            | Kanta
            deriving (Show, Eq)

data Lunch = JapaneseCurry
           | Ramen
           | Soba
           deriving (Show, Eq)

allPerson :: [Person]
allPerson =  [Tonchiki, Chinpei, Kanta]

allLunch :: [Lunch]
allLunch =  [JapaneseCurry, Ramen, Soba]

allCases :: [([Person], [Lunch])]
allCases =  [(allPerson, l) | l <- permutations allLunch]

question :: Person -> Lunch -> Maybe Bool
question =  d  where
  d Tonchiki _             = Nothing
  d Chinpei  Soba          = Just False
  d Kanta    JapaneseCurry = Just False
  d Kanta    Soba          = Just False
  d _        _             = Nothing

goodCase :: ([Person], [Lunch]) -> Bool
goodCase (ps, ls) = all (/= Just False) $ zipWith question ps ls

good :: [([Person], [Lunch])]
good =  filter goodCase allCases
