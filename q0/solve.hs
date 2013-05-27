
import L1

-- 制約から得られた事実
f0 :: Maybe (Either (NotHasLunch Chinpei Soba) (HasLunch Chinpei Soba))
f0 =  question Chinpei' Soba'

f1 :: Maybe (Either (NotHasLunch Kanta JapaneseCurry) (HasLunch Kanta JapaneseCurry))
f1 =  question Kanta' JapaneseCurry'

f2 :: Maybe (Either (NotHasLunch Kanta Soba) (HasLunch Kanta Soba))
f2 =  question Kanta' Soba'

-- 事実から推論された結論
solve :: (HasLunch Kanta Ramen, HasLunch Chinpei JapaneseCurry, HasLunch Tonchiki Soba)
solve =
  let (Just (Left chinpeiNotSoba)) = f0
      (Just (Left kantaNotCurry))  = f1
      (Just (Left kantaNotSoba))   = f2
      kanta = ramen kantaNotSoba kantaNotCurry
      (tonchikiNotRamen, chinpeiNotRamen) = kantaHasRamen kanta
      chinpei = japaneseCurry chinpeiNotRamen chinpeiNotSoba
      (kantaNotCuryy, tonchikiNotCurry) = chinpeiHasJapaneseCurry chinpei
      tonchiki = soba tonchikiNotCurry tonchikiNotRamen
  in  (kanta, chinpei, tonchiki)


main = undefined
