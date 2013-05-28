module Even1 where

data Even = Double Int deriving Show

double :: Int -> Even
double i = Double i

mayEven :: Int -> Maybe Even
mayEven i | rm == 0   = Just (double qt)
          | otherwise = Nothing
  where (qt, rm) = i `quotRem` 2
