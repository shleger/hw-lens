{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Exercise1 where

import Lens.Micro.Platform
import Data.Text (Text)
import Test.Hspec


data Address = Address
  { _street :: !Text
  , _city :: !Text
  }

makeLenses ''Address

data Person = Person
  { _name :: !Text
  , _address :: !Address
  , _age :: !Int
  }

makeLenses ''Person

hollywood :: Text
hollywood = "Hollywood Blvd"

alice :: Person
alice = Person
  { _name = "Alice"
  , _address = Address
      { _street = hollywood
      , _city = "Los Angeles"
      }
  , _age = 30
  }

wilshire :: Text
wilshire = "Wilshire Blvd"

-- -- FIXME set Alice's street to Wilshire
aliceWilshire :: Person
aliceWilshire  = setStreet alice
-- aliceWilshire  = set  (address . street )  wilshire  alice

setStreet :: Person -> Person
setStreet = set  (address . street )  wilshire 

getStreet :: Person -> Text
getStreet = view $ address . street

-- | Increase age by 1
birthday :: Person -> Person
birthday = over age (+1)  -- the same as: birthday person = over age (+1) person

getAge :: Person -> Int
getAge = view age
-- the same:
-- getAge person = person ^. age

test1 :: IO ()
test1 = hspec $ do
  it "lives on Wilshire" $
    _street (_address aliceWilshire) `shouldBe` wilshire
  it "getStreet works" $
    getStreet alice `shouldBe` hollywood
  it "birthday" $
    getAge (birthday alice) `shouldBe` _age alice + 1