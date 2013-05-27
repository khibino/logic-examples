module L1 (
  module Class,

  HasLunch, NotHasLunch,

  japaneseCurry, soba, ramen,

  kantaHasRamen, chinpeiHasJapaneseCurry,

  question
  ) where

import qualified L0
import Class


data HasLunch p l    = HasLunch ()    deriving Show

data NotHasLunch p l = NotHasLunch () deriving Show


-- 公理
japaneseCurry :: NotHasLunch p Ramen -> NotHasLunch p Soba -> HasLunch p JapaneseCurry
japaneseCurry (NotHasLunch ()) (NotHasLunch ()) = HasLunch ()

soba :: NotHasLunch p JapaneseCurry -> NotHasLunch p Ramen -> HasLunch p Soba
soba (NotHasLunch ()) (NotHasLunch ()) = HasLunch ()

ramen :: NotHasLunch p Soba -> NotHasLunch p JapaneseCurry -> HasLunch p Ramen
ramen (NotHasLunch ()) (NotHasLunch ()) = HasLunch ()

kantaHasRamen :: HasLunch Kanta Ramen -> (NotHasLunch Tonchiki Ramen, NotHasLunch Chinpei Ramen)
kantaHasRamen (HasLunch ()) = (NotHasLunch (), NotHasLunch ())

chinpeiHasJapaneseCurry :: HasLunch Chinpei JapaneseCurry -> (NotHasLunch Kanta JapaneseCurry, NotHasLunch Tonchiki JapaneseCurry)
chinpeiHasJapaneseCurry (HasLunch ()) = (NotHasLunch (), NotHasLunch ())

-- 制約
question :: (Person p, Lunch l) => p -> l -> Maybe (Either (NotHasLunch p l) (HasLunch p l))
question p l
  | qr == Just True  = Just . Right $ HasLunch ()
  | qr == Just False = Just . Left  $ NotHasLunch ()
  | otherwise        = Nothing
  where qr = L0.question (unSpecifyPerson p) (unSpecifyLunch l)
