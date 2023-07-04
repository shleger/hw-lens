{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

-- https://www.fpcomplete.com/haskell/tutorial/lens/
module Polymorphic where

import Data.Text (Text)
import qualified Data.Text as T

type Lens s a = forall f. Functor f => (a -> f a) -> (s -> f s)

-- ============================

data Person age = Person
  { personName :: !Text,
    personAge :: !age
  }

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter f s = setter s <$> f (getter s)  

aliceInt :: Person Int
aliceInt = Person "Alice" 30

personAgeL :: Lens (Person age) age
personAgeL = lens personAge (\x y -> x {personAge = y})

setAge :: age -> Person oldAge -> Person age
setAge age person = person {personAge = age}

aliceDouble :: Person Double
aliceDouble = setAge 30.5 aliceInt

-- alice = Person {personName = "name", personAge = 18}

main :: IO ()
main = do


  -- putStrLn ("out: " ++ show (setPersonAddress  Address{addressCity="Voronezh", addressStreet="New Street"} alice ))
  -- putStrLn ("out: " ++ show (  view   personCityL alice   ))
  -- putStrLn ("out: " ++ show (  reverseCity alice   ))
  -- putStrLn ("out: " ++ show (  setCity "Voronezh" alice   ))
  print "ddd"
