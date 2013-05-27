module Class where

import qualified L0

data Tonchiki = Tonchiki'  deriving (Show, Eq)
data Chinpei  = Chinpei'   deriving (Show, Eq)
data Kanta    = Kanta'     deriving (Show, Eq)

data JapaneseCurry = JapaneseCurry'  deriving (Show, Eq)
data Ramen         = Ramen'          deriving (Show, Eq)
data Soba          = Soba'           deriving (Show, Eq)


data SpecifiedPerson p =
  SpecifiedPerson
  { justPerson        :: p
  , unSpecifiedPerson :: L0.Person
  }

class Person p where
  person     :: SpecifiedPerson p

specifyPerson' :: SpecifiedPerson p -> L0.Person -> Maybe p
specifyPerson' spp p
  | unSpecifiedPerson spp == p = Just (justPerson spp)
  | otherwise                  = Nothing

specifyPerson :: Person p => L0.Person -> Maybe p
specifyPerson p = specifyPerson' person p

unSpecifyPerson' :: SpecifiedPerson p -> p -> L0.Person
unSpecifyPerson' spp _ = unSpecifiedPerson spp

unSpecifyPerson :: Person p => p -> L0.Person
unSpecifyPerson =  unSpecifyPerson' person


instance Person Tonchiki where
  person = SpecifiedPerson Tonchiki' L0.Tonchiki

instance Person Chinpei where
  person = SpecifiedPerson Chinpei' L0.Chinpei

instance Person Kanta where
  person = SpecifiedPerson Kanta' L0.Kanta


data SpecifiedLunch l =
  SpecifiedLunch
  { justLunch        :: l
  , unSpecifiedLunch :: L0.Lunch
  }

class Lunch l where
  lunch      :: SpecifiedLunch l

specifyLunch' :: SpecifiedLunch l -> L0.Lunch -> Maybe l
specifyLunch' spl l
  | unSpecifiedLunch spl == l = Just (justLunch spl)
  | otherwise                 = Nothing

specifyLunch :: Lunch l => L0.Lunch -> Maybe l
specifyLunch l = specifyLunch' lunch l

unSpecifyLunch' :: SpecifiedLunch l -> l -> L0.Lunch
unSpecifyLunch' spl _ = unSpecifiedLunch spl

unSpecifyLunch :: Lunch l => l -> L0.Lunch
unSpecifyLunch =  unSpecifyLunch' lunch

instance Lunch JapaneseCurry where
  lunch = SpecifiedLunch JapaneseCurry' L0.JapaneseCurry

instance Lunch Ramen where
  lunch = SpecifiedLunch Ramen' L0.Ramen

instance Lunch Soba where
  lunch = SpecifiedLunch Soba' L0.Soba
