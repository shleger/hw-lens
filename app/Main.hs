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

setPersonCity :: Text -> Person -> Person
setPersonCity city person =
  person
    { personAddress =
        (personAddress person)
          { addressCity = city
          }
    }

main :: IO ()
main = do
  let alice = Person {personName = "name", personAddress = Address {addressCity = "Moscow", addressStreet = "Arbat"}}

  putStrLn ("out: " ++ show (setPersonCity "Voronezh" alice))
