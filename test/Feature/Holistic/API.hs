{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.Holistic.API
  ( api
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (ENUM, INPUT_OBJECT, KIND, OBJECT, SCALAR)
import           Data.Morpheus.Types        ((::->), GQLArgs, GQLMutation, GQLQuery, GQLRootResolver (..),
                                             GQLScalar (..), GQLSubscription, GQLType (..), ID (..), ScalarValue (..))
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
  , street      :: Maybe [Maybe[[[Text]]]]
  , houseNumber :: Int
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

data Query = Query
  { user    :: () ::-> User
  , fieldId :: ID
  } deriving (Generic, GQLQuery)

newtype Mutation = Mutation
  { createUser :: AddressArgs ::-> User
  } deriving (Generic, GQLMutation)

newtype Subscription = Subscription
  { newUser :: AddressArgs ::-> User
  } deriving (Generic, GQLSubscription)

resolveAddress :: a ::-> Address
resolveAddress = return Address {city = "", houseNumber = 1, street = Nothing}

resolveUser :: a ::-> User
resolveUser =
  return $
  User
    { name = "testName"
    , email = ""
    , address = resolveAddress
    , office = resolveAddress
    , home = HH
    , friend = return Nothing
    }

createUserMutation :: AddressArgs ::-> User
createUserMutation = resolveUser

newUserSubscription :: AddressArgs ::-> User
newUserSubscription = resolveUser

api :: ByteString -> IO ByteString
api =
  interpreter
    GQLRootResolver
      { queryResolver = Query {user = resolveUser, fieldId = ID ""}
      , mutationResolver = Mutation {createUser = createUserMutation}
      , subscriptionResolver = Subscription {newUser = newUserSubscription}
      }
