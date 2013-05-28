module Even0 (Even, mayEven)  where

-- data Maybe a = Just a
--              | Nothing

data Even = Even' Int deriving Show

mayEven :: Int -> Maybe Even
mayEven i
  | i `rem` 2 == 0 = Just (Even' i)
  | otherwise      = Nothing
-- 例えば Even' は他からは隠蔽する
