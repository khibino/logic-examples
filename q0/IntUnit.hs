module IntUnit where

data Int' u  = Int' Int

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
