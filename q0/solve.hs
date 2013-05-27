
import qualified L0
import Class
import L1


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

question :: (Person p, Lunch l) => p -> l -> Maybe (Either (NotHasLunch p l) (HasLunch p l))
question p l
  | qr == Just True  = Just . Right $ HasLunch ()
  | qr == Just False = Just . Left  $ NotHasLunch ()
  | otherwise        = Nothing
  where qr = L0.question (unSpecifyPerson p) (unSpecifyLunch l)


f0' :: Maybe (Either (NotHasLunch Kanta JapaneseCurry) (HasLunch Kanta JapaneseCurry))
f0' =  question Kanta' JapaneseCurry'

f1' :: Maybe (Either (NotHasLunch Kanta Soba) (HasLunch Kanta Soba))
f1' =  question Kanta' Soba'

f2' :: Maybe (Either (NotHasLunch Chinpei Soba) (HasLunch Chinpei Soba))
f2' =  question Chinpei' Soba'


solve :: (HasLunch Kanta Ramen, HasLunch Chinpei JapaneseCurry, HasLunch Tonchiki Soba)
solve =
  let (Just (Left f0)) = question Kanta' JapaneseCurry'
      (Just (Left f1)) = question Kanta' Soba'
      (Just (Left f2)) = question Chinpei' Soba'
      kanta = ramen f1 f0
      (tonchikiNotRamen, chinpeiNotRamen) = kantaHasRamen kanta
      chinpei = japaneseCurry chinpeiNotRamen f2
      (kantaNotCuryy, tonchikiNotCurry) = chinpeiHasJapaneseCurry chinpei
      tonchiki = soba tonchikiNotCurry tonchikiNotRamen
  in  (kanta, chinpei, tonchiki)


main = undefined
