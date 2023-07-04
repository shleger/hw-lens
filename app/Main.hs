{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)

data Address = Address
  { addressCity :: !Text,
    addressStreet :: !Text
  }
  deriving (Show)

data Person = Person
  { personAddress :: !Address,
    personName :: !Text
  }
  deriving (Show)

getPersonCity :: Person -> Text
getPersonCity = addressCity . personAddress

-- setPersonCity :: Text -> Person -> Person
-- setPersonCity city person =
--   person
--     { personAddress =
--         (personAddress person)
--           { addressCity = city
--           }
--     }

modifyAddressCity :: (Text -> Text) -> Address -> Address
modifyAddressCity f address = address { addressCity = f (addressCity address) }

modifyPersonAddress :: (Address -> Address) -> Person -> Person
modifyPersonAddress f person = person { personAddress = f (personAddress person)}

modifyPersonCity :: (Text -> Text) -> Person -> Person
modifyPersonCity = modifyPersonAddress . modifyAddressCity

-- setPersonCity :: Text -> Person -> Person
-- setPersonCity city = modifyPersonCity (const city)


data Lens s a = Lens
    { lensGetter :: s -> a
    , lensModify :: (a -> a) -> s -> s
    }

composeLens :: Lens a b -> Lens b c -> Lens a c
composeLens (Lens getter1 modify1) (Lens getter2 modify2) = Lens
    { lensGetter = getter2 . getter1
    , lensModify = modify1 . modify2
    }


personAddressL :: Lens Person Address
personAddressL = Lens
    { lensGetter = personAddress
    , lensModify = \f person -> person { personAddress = f (personAddress person) }
    }

addressCityL :: Lens Address Text
addressCityL = Lens
    { lensGetter = addressCity
    , lensModify = \f address -> address { addressCity = f (addressCity address) }
    }

personCityL :: Lens Person Text
personCityL = personAddressL `composeLens` addressCityL

setPersonCity :: Text -> Person -> Person
setPersonCity city = lensModify personCityL (const city)    




-- ============================

main :: IO ()
main = do
  let alice = Person {personName = "name", personAddress = Address {addressCity = "Moscow", addressStreet = "Arbat"}}

  putStrLn ("out: " ++ show (setPersonCity  "Voronezh" alice ))
