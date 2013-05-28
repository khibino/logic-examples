module Maybe where

justOne :: Maybe Integer
justOne =  Just 1

justHello :: Maybe String
justHello =  Just "Hello"

noInt :: Maybe Int
noInt =  Nothing

noString :: Maybe String
noString =  Nothing

-- noInt == noString
