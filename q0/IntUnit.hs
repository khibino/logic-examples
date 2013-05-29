module IntUnit where

data Int' u  = Int' Int deriving (Show, Eq)

(<+>) :: Int' u -> Int' u -> Int' u
Int' a <+> Int' b = Int' (a + b)

data YenUnit  = Yen
type Yen  = Int' YenUnit

data GramUnit = Gram
type Gram = Int' GramUnit

twentyYen :: Yen
twentyYen =  Int' 20

thirtyGram :: Gram
thirtyGram =  Int' 30

-- twentyYen <+> thirtyGram

data ImposedUnit = Imposed
type Imposed = Int' ImposedUnit

impose :: Yen -> Imposed
impose (Int' i) = Int' (floor $ fromIntegral i * (1.05 :: Rational) )

twentyImposed :: Imposed
twentyImposed =  impose twentyYen

-- twentyYen <+> twentyImposed
