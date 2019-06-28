{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Deprecated.API
  ( gqlRoot
  ) where

import           Data.Morpheus.Kind  (ENUM, INPUT_OBJECT, KIND, OBJECT, SCALAR, UNION)
import           Data.Morpheus.Types (BaseR, EffectR, GQLRootResolver (..), GQLScalar (..), GQLType (..), ID, Resolver,
                                      ScalarValue (..), gqlEffectResolver, gqlResolver)
import           Data.Text           (Text)
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)

type instance KIND CityID = ENUM

type instance KIND Euro = SCALAR

type instance KIND UID = INPUT_OBJECT

type instance KIND Coordinates = INPUT_OBJECT

type instance KIND Address = OBJECT

type instance KIND (User res) = OBJECT

type instance KIND (MyUnion res) = UNION

data MyUnion res
  = USER (User res)
  | ADDRESS Address
  deriving (Generic, GQLType)

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
  } deriving (Show, Generic, GQLType)

data Coordinates = Coordinates
  { latitude  :: Euro
  , longitude :: [Maybe [[UID]]]
  } deriving (Generic)

instance GQLType Coordinates where
  description _ = "just random latitude and longitude"

data Address = Address
  { city        :: Text
  , street      :: Text
  , houseNumber :: Int
  } deriving (Generic, GQLType)

data AddressArgs = AddressArgs
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic)

data OfficeArgs = OfficeArgs
  { zipCode :: Maybe [[Maybe [ID]]]
  , cityID  :: CityID
  } deriving (Generic)

data User m = User
  { name    :: Text
  , email   :: Text
  , address :: AddressArgs -> m Address
  , office  :: OfficeArgs -> m Address
  , myUnion :: () -> m (MyUnion m)
  , home    :: CityID
  } deriving (Generic)

instance Typeable a => GQLType (User a) where
  description _ = "Custom Description for Client Defined User Type"

type instance KIND (A Int) = OBJECT

type instance KIND (A Text) = OBJECT

newtype A a = A
  { wrappedA :: a
  } deriving (Generic, GQLType)

data Query = Query
  { user      :: () -> BaseR (User BaseR)
  , wrappedA1 :: A Int
  , wrappedA2 :: A Text
  } deriving (Generic)

fetchAddress :: Monad m => Euro -> m (Either String Address)
fetchAddress _ = return $ Right $ Address " " "" 0

fetchUser :: Monad m => m (Either String (User (Resolver m)))
fetchUser =
  return $
  Right $
  User
    { name = "George"
    , email = "George@email.com"
    , address = const resolveAddress
    , office = resolveOffice
    , home = HH
    , myUnion = const $ return $ USER unionUser
    }
  where
    unionAddress = Address {city = "Hamburg", street = "Street", houseNumber = 20}
    -- Office
    resolveOffice OfficeArgs {cityID = Paris} = gqlResolver $ fetchAddress (Euro 1 1)
    resolveOffice OfficeArgs {cityID = BLN}   = gqlResolver $ fetchAddress (Euro 1 2)
    resolveOffice OfficeArgs {cityID = HH}    = gqlResolver $ fetchAddress (Euro 1 3)
    resolveAddress = gqlResolver $ fetchAddress (Euro 1 0)
    unionUser =
      User
        { name = "David"
        , email = "David@email.com"
        , address = const resolveAddress
        , office = resolveOffice
        , home = BLN
        , myUnion = const $ return $ ADDRESS unionAddress
        }

createUserMutation :: a -> EffectR (User EffectR)
createUserMutation _ = gqlEffectResolver ["UPDATE_USER"] fetchUser

newUserSubscription :: a -> EffectR (User EffectR)
newUserSubscription _ = gqlEffectResolver ["UPDATE_USER"] fetchUser

createAddressMutation :: a -> EffectR Address
createAddressMutation _ = gqlEffectResolver ["UPDATE_ADDRESS"] (fetchAddress (Euro 1 0))

newAddressSubscription :: a -> EffectR Address
newAddressSubscription _ = gqlEffectResolver ["UPDATE_ADDRESS"] $ fetchAddress (Euro 1 0)

data Mutation = Mutation
  { createUser    :: () -> EffectR (User EffectR)
  , createAddress :: () -> EffectR Address
  } deriving (Generic)

data Subscription = Subscription
  { newUser    :: () -> EffectR (User EffectR)
  , newAddress :: () -> EffectR Address
  } deriving (Generic)

gqlRoot :: GQLRootResolver Query Mutation Subscription
gqlRoot =
  GQLRootResolver
    { queryResolver = Query {user = const $ gqlResolver fetchUser, wrappedA1 = A 0, wrappedA2 = A ""}
    , mutationResolver = Mutation {createUser = createUserMutation, createAddress = createAddressMutation}
    , subscriptionResolver = Subscription {newUser = newUserSubscription, newAddress = newAddressSubscription}
    }
