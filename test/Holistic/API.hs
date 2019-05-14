{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Holistic.API
  ( api
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (ENUM, GQLArgs, GQLSubscription, GQLMutation, GQLQuery, GQLScalar (..), GQLType (..),
                                             INPUT_OBJECT, KIND, OBJECT, SCALAR)
import           Data.Morpheus.Types        ((::->) (..), GQLRoot (..), ScalarValue (..))
import           Data.Text                  (Text)
import           GHC.Generics               (Generic)

type instance KIND CityID = ENUM

type instance KIND Euro = SCALAR

type instance KIND UID = INPUT_OBJECT

type instance KIND Coordinates = INPUT_OBJECT

type instance KIND Address = OBJECT

type instance KIND User = OBJECT

data CityID
  = Paris
  | BLN
  | HH
  deriving (Generic, GQLType)

data Euro =
  Euro Int
       Int
  deriving (Generic, GQLType)

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

newtype UID = UID
  { uid :: Text
  } deriving (Generic, GQLType)

data Coordinates = Coordinates
  { latitude  :: Euro
  , longitude :: [UID]
  } deriving (Generic, GQLType)

data Address = Address
  { city        :: Text
  , street      :: Text
  , houseNumber :: Int
  , owner       :: Maybe User
  } deriving (Generic, GQLType)

data AddressArgs = AddressArgs
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic, GQLArgs)

data OfficeArgs = OfficeArgs
  { zipCode :: Maybe [Int]
  , cityID  :: CityID
  } deriving (Generic, GQLArgs)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: AddressArgs ::-> Address
  , office  :: OfficeArgs ::-> Address
  , friend  :: () ::-> Maybe User
  , home    :: CityID
  } deriving (Generic)

instance GQLType User where
  description _ = "Custom Description for Client Defined User Type"

newtype Query = Query
  { user :: () ::-> User
  } deriving (Generic, GQLQuery)

newtype Mutation = Mutation
  { createUser :: AddressArgs ::-> User
  } deriving (Generic, GQLMutation)

newtype Subscription = Subscription
  { newUser :: AddressArgs ::-> User
  } deriving (Generic, GQLSubscription)

resolveAddress :: a ::-> Address
resolveAddress = return Address {city = "", houseNumber = 1, street = "", owner = Nothing}

resolveUser :: a ::-> User
resolveUser =
  return $
  User {name = "", email = "", address = resolveAddress, office = resolveAddress, home = HH, friend = return Nothing}

createUserMutation :: AddressArgs ::-> User
createUserMutation = resolveUser

newUserSubscription :: AddressArgs ::-> User
newUserSubscription = resolveUser

api :: ByteString -> IO ByteString
api = interpreter GQLRoot {query = Query {user = resolveUser}, mutation = Mutation {createUser = createUserMutation}, subscription = Subscription {newUser = newUserSubscription}}
