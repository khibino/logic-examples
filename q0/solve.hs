
import L1

-- 制約から得られた事実
chinpeiNotSoba :: NotHasLunch Chinpei Soba
Just (Left chinpeiNotSoba) = question Chinpei' Soba'

kantaNotCurry :: NotHasLunch Kanta JapaneseCurry
Just (Left kantaNotCurry) =  question Kanta' JapaneseCurry'

kantaNotSoba :: NotHasLunch Kanta Soba
Just (Left kantaNotSoba) = question Kanta' Soba'

-- 事実から推論された結論
solve :: (HasLunch Kanta Ramen, HasLunch Chinpei JapaneseCurry, HasLunch Tonchiki Soba)
solve =
  let kanta = ramen kantaNotSoba kantaNotCurry
      (tonchikiNotRamen, chinpeiNotRamen) = kantaHasRamen kanta
      chinpei = japaneseCurry chinpeiNotRamen chinpeiNotSoba
      (kantaNotCuryy, tonchikiNotCurry) = chinpeiHasJapaneseCurry chinpei
      tonchiki = soba tonchikiNotCurry tonchikiNotRamen
  in  (kanta, chinpei, tonchiki)


main = undefined
