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

type instance KIND TestScalar = SCALAR

type instance KIND NestedInputObject = INPUT_OBJECT

type instance KIND Coordinates = INPUT_OBJECT

type instance KIND TestInputObject = INPUT_OBJECT

type instance KIND Address = OBJECT

type instance KIND User = OBJECT

data CityID
  = Paris
  | BLN
  | HH
  deriving (Generic, GQLType)

data TestScalar =
  TestScalar Int
             Int
  deriving (Generic, GQLType)

instance GQLScalar TestScalar where
  parseValue _ = pure (TestScalar 1 0)
  serialize (TestScalar x y) = Int (x * 100 + y)

newtype NestedInputObject = NestedInputObject
  { fieldTestID :: ID
  } deriving (Generic, GQLType)

data TestInputObject = TestInputObject
  { fieldTestScalar        :: TestScalar
  , fieldNestedInputObject :: [Maybe NestedInputObject]
  } deriving (Generic, GQLType)

data StreetArgs = StreetArgs
  { argInputObject :: TestInputObject
  , argMaybeString :: Maybe Text
  } deriving (Generic, GQLArgs)

data Address = Address
  { city        :: Text
  , street      :: StreetArgs ::-> Maybe [Maybe [[[Text]]]]
  , houseNumber :: Int
  } deriving (Generic, GQLType)

data Coordinates = Coordinates
  { latitude  :: TestScalar
  , longitude :: Int
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
resolveAddress = return Address {city = "", houseNumber = 1, street = return Nothing}

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
