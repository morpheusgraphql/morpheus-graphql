{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.Holistic.API
  ( api
  ) where

import           Data.Morpheus       (interpreter)
import           Data.Morpheus.Kind  (ENUM, INPUT_OBJECT, OBJECT, SCALAR, UNION)
import           Data.Morpheus.Types (Event (..), GQLRequest, GQLResponse, GQLRootResolver (..), GQLScalar (..),
                                      GQLType (..), ID (..), IORes, IOSubRes, ScalarValue (..))
import           Data.Text           (Text)
import           GHC.Generics        (Generic)

data TestEnum
  = EnumA
  | EnumB
  | EnumC
  deriving (Generic)

instance GQLType TestEnum where
  type KIND TestEnum = ENUM

data TestScalar =
  TestScalar Int
             Int
  deriving (Generic)

instance GQLType TestScalar where
  type KIND TestScalar = SCALAR

instance GQLScalar TestScalar where
  parseValue _ = pure (TestScalar 1 0)
  serialize (TestScalar x y) = Int (x * 100 + y)

newtype NestedInputObject = NestedInputObject
  { fieldTestID :: ID
  } deriving (Generic)

instance GQLType NestedInputObject where
  type KIND NestedInputObject = INPUT_OBJECT

data TestInputObject = TestInputObject
  { fieldTestScalar        :: TestScalar
  , fieldNestedInputObject :: [Maybe NestedInputObject]
  } deriving (Generic)

instance GQLType TestInputObject where
  type KIND TestInputObject = INPUT_OBJECT

data StreetArgs = StreetArgs
  { argInputObject :: TestInputObject
  , argMaybeString :: Maybe Text
  } deriving (Generic)

data Address = Address
  { city        :: Text
  , street      :: StreetArgs -> IORes (Maybe [Maybe [[[Text]]]])
  , houseNumber :: Int
  } deriving (Generic)

instance GQLType Address where
  type KIND Address = OBJECT

data TestUnion
  = UnionA User
  | UnionB Address
  deriving (Generic)

instance GQLType TestUnion where
  type KIND TestUnion = UNION

data Coordinates = Coordinates
  { latitude  :: TestScalar
  , longitude :: Int
  } deriving (Generic)

instance GQLType Coordinates where
  type KIND Coordinates = INPUT_OBJECT

data AddressArgs = AddressArgs
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic)

data OfficeArgs = OfficeArgs
  { zipCode :: Maybe [Int]
  , cityID  :: TestEnum
  } deriving (Generic)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: AddressArgs -> IORes Address
  , office  :: OfficeArgs -> IORes Address
  , friend  :: () -> IORes (Maybe User)
  } deriving (Generic)

instance GQLType User where
  type KIND User = OBJECT
  description = const "Custom Description for Client Defined User Type"

data Query = Query
  { user      :: () -> IORes User
  , testUnion :: Maybe TestUnion
  } deriving (Generic)

newtype Mutation = Mutation
  { createUser :: AddressArgs -> IORes User
  } deriving (Generic)

data EVENT =
  EVENT
  deriving (Show, Eq)

newtype Subscription = Subscription
  { newUser :: AddressArgs -> IOSubRes EVENT () User
  } deriving (Generic)

resolveAddress :: a -> IORes Address
resolveAddress _ = return Address {city = "", houseNumber = 0, street = const $ return Nothing}

resolveUser :: a -> IORes User
resolveUser _ =
  return $
  User
    {name = "testName", email = "", address = resolveAddress, office = resolveAddress, friend = const $ return Nothing}

rootResolver :: GQLRootResolver IO EVENT () Query Mutation Subscription
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {user = resolveUser, testUnion = Nothing}
    , mutationResolver = return Mutation {createUser = resolveUser}
    , subscriptionResolver = return Subscription {newUser = const $ Event [EVENT] resolveUser}
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
