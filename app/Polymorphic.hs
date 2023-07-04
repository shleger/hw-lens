{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}

-- https://www.fpcomplete.com/haskell/tutorial/lens/
module Polymorphic where

import Data.Text (Text)
import qualified Data.Text as T

type Lens s a =  forall f. Functor f => (a -> f a) -> (s -> f s)
-- type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

-- -- Our old monomorphic variant
-- type Lens' s a = Lens s s a a



-- ============================

data Person age = Person
  { personName :: !Text,
    personAge :: !age
  } deriving Show

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter f s = setter s <$> f (getter s)

newtype Identity a = Identity { runIdentity :: a } deriving Functor
newtype Const a b = Const { getConst :: a } deriving Functor

over :: Lens s a -> (a -> a) -> s -> s
over lens f s = runIdentity (lens (Identity . f) s)

view :: Lens s a -> s -> a
view lens s = getConst (lens Const s)

(^.) :: s -> Lens s a -> a
s ^. lens = view lens s

(%~) :: Lens s a -> (a -> a) -> s -> s
(%~) = over

aliceInt :: Person Int
aliceInt = Person "Alice" 30

personAgeL :: Lens (Person age) age
personAgeL = lens personAge (\x y -> x {personAge = y})

setAge :: age -> Person oldAge -> Person age
setAge age person = person {personAge = age}

aliceDouble :: Person Double
aliceDouble = setAge 30.5 aliceInt

alice = Person {personName = "Alice2", personAge = 18}

main :: IO ()
main = do

  putStrLn ("out: " ++ show (  aliceInt ^. personAgeL    ))
  putStrLn ("out: " ++ show (  alice ^. personAgeL    ))
  putStrLn ("out: " ++ show (  view personAgeL alice   ))
  putStrLn ("out: " ++ show aliceDouble)  -- should fail in tutorial
  putStrLn ("out: " ++ show (setAge 30.5 alice)) -- should fail in tutorial

