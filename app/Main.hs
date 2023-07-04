{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}


-- https://www.fpcomplete.com/haskell/tutorial/lens/
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

-- modifyAddressCity :: (Text -> Text) -> Address -> Address
-- modifyAddressCity f address = address { addressCity = f (addressCity address) }

-- modifyPersonAddress :: (Address -> Address) -> Person -> Person
-- modifyPersonAddress f person = person { personAddress = f (personAddress person)}

-- modifyPersonCity :: (Text -> Text) -> Person -> Person
-- modifyPersonCity = modifyPersonAddress . modifyAddressCity

-- setPersonCity :: Text -> Person -> Person
-- setPersonCity city = modifyPersonCity (const city)


-- data Lens s a = Lens
--     { lensGetter :: s -> a
--     , lensModify :: (a -> a) -> s -> s
--     }

-- composeLens :: Lens a b -> Lens b c -> Lens a c
-- composeLens (Lens getter1 modify1) (Lens getter2 modify2) = Lens
--     { lensGetter = getter2 . getter1
--     , lensModify = modify1 . modify2
--     }


-- personAddressL :: Lens Person Address
-- personAddressL = Lens
--     { lensGetter = personAddress
--     , lensModify = \f person -> person { personAddress = f (personAddress person) }
--     }

-- addressCityL :: Lens Address Text
-- addressCityL = Lens
--     { lensGetter = addressCity
--     , lensModify = \f address -> address { addressCity = f (addressCity address) }
--     }

-- personCityL :: Lens Person Text
-- personCityL = personAddressL `composeLens` addressCityL

-- setPersonCity :: Text -> Person -> Person
-- setPersonCity city = lensModify personCityL (const city)    


-- ============================
-- newtype Identity a = Identity { runIdentity :: a } deriving Functor

-- type LensModify s a = (a -> Identity a) -> (s -> Identity s)

-- over :: LensModify s a -> (a -> a) -> s -> s
-- over lens f s = runIdentity (lens (Identity . f) s)

-- personAddressL :: LensModify Person Address
-- personAddressL f person = Identity $ person { personAddress = runIdentity $ f $ personAddress person }

-- personAddressL :: LensModify Person Address
-- personAddressL f person = (\address -> person { personAddress = address }) <$> f (personAddress person)


-- type LensGetter s a = s -> Const a s

-- view :: LensGetter s a -> s -> a
-- view lens s = getConst (lens s)

-- personAddressL :: LensGetter Person Address
-- personAddressL person = Const (personAddress person)

-- ============================
-- newtype Const a b = Const { getConst :: a } deriving Functor


-- type LensModify s a = (a -> Identity a) -> (s -> Identity s)
-- type LensGetter s a = s -> Const a s


-- type LensGetter s a = (a -> Const a s) -> (s -> Const a s)

-- view :: LensGetter s a -> s -> a
-- view lens s = getConst (lens Const s)

-- personAddressL :: LensGetter Person Address
-- personAddressL f person = Const $ getConst $ f (personAddress person)

-- personAddressL :: LensModify Person Address
-- personAddressL f person = (\address -> person { personAddress = address }) <$> f (personAddress person)

-- ============================Complex lens)
type Lens s a = forall f. Functor f => (a -> f a) -> (s -> f s)

newtype Identity a = Identity { runIdentity :: a } deriving Functor

newtype Const a b = Const { getConst :: a } deriving Functor

over :: Lens s a -> (a -> a) -> s -> s
over lens f s = runIdentity (lens (Identity . f) s)

view :: Lens s a -> s -> a
view lens s = getConst (lens Const s)

-- personAddressL :: Lens Person Address
-- personAddressL f person = (\address -> person { personAddress = address }) <$> f (personAddress person)

getPersonAddress :: Person -> Address
getPersonAddress = view personAddressL

modifyPersonAddress :: (Address -> Address) -> Person -> Person
modifyPersonAddress = over personAddressL

setPersonAddress :: Address -> Person -> Person
setPersonAddress address = modifyPersonAddress (const address)

-- ============================ Composing lenses

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter f s = setter s <$> f (getter s)

personAddressL :: Lens Person Address
personAddressL = lens personAddress (\x y -> x { personAddress = y })

addressCityL :: Lens Address Text
addressCityL = lens addressCity (\x y -> x { addressCity = y })

personCityL :: Lens Person Text
personCityL = personAddressL . addressCityL

-- (^.) :: s -> Lens s a -> a
-- s ^. lens = view lens s

-- ============================    

main :: IO ()
main = do
  let alice = Person {personName = "name", personAddress = Address {addressCity = "Moscow", addressStreet = "Arbat"}}

  putStrLn ("out: " ++ show (setPersonAddress  Address{addressCity="Voronezh", addressStreet="New Street"} alice ))
  putStrLn ("out: " ++ show (  view   personCityL alice  ))
