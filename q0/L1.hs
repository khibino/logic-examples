module L1 where

data Tonchiki = Tonchiki'  deriving (Show, Eq)
data Chinpei  = Chinpei'   deriving (Show, Eq)
data Kanta    = Kanta'     deriving (Show, Eq)

data JapaneseCurry = JapaneseCurry'  deriving (Show, Eq)
data Ramen         = Ramen'          deriving (Show, Eq)
data Soba          = Soba'           deriving (Show, Eq)


data HasLunch p l    = HasLunch ()    deriving Show

data NotHasLunch p l = NotHasLunch () deriving Show
