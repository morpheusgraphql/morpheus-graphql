{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Deprecated.API
  ( gqlRoot
  ) where

import           Data.Map            (Map)
import qualified Data.Map            as M (fromList)
import           Data.Set            (Set)
import qualified Data.Set            as S (fromList)
import           Data.Text           (Text, pack)
import           Data.Typeable       (Typeable)
import           GHC.Generics        (Generic)

-- MORPHEUS
import           Data.Morpheus.Kind  (ENUM, INPUT_OBJECT, INPUT_UNION, KIND, OBJECT, SCALAR, UNION)
import           Data.Morpheus.Types (EffectM, GQLRootResolver (..), GQLScalar (..), GQLType (..), ID, ResM, Resolver,
                                      ScalarValue (..), gqlEffectResolver, gqlResolver)

type instance KIND CityID = ENUM

type instance KIND Euro = SCALAR

type instance KIND UniqueID = INPUT_OBJECT

type instance KIND Coordinates = INPUT_OBJECT

type instance KIND Address = OBJECT

type instance KIND (User res) = OBJECT

type instance KIND (MyUnion res) = UNION

type instance KIND Foo = INPUT_OBJECT

type instance KIND InputUnion = INPUT_UNION

data InputUnion
  = UID UniqueID
  | FOO Foo
  deriving (Show, Generic, GQLType)

newtype UniqueID = UniqueID
  { uid :: Text
  } deriving (Show, Generic, GQLType)

newtype Foo = Foo
  { fooField :: Text
  } deriving (Show, Generic, GQLType)

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

data Coordinates = Coordinates
  { latitude  :: Euro
  , longitude :: [Maybe [[UniqueID]]]
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

type instance KIND (A a) = OBJECT

newtype A a = A
  { wrappedA :: a
  } deriving (Generic, GQLType)

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

createUserMutation :: a -> EffectM (User EffectM)
createUserMutation _ = gqlEffectResolver ["UPDATE_USER"] fetchUser

newUserSubscription :: a -> EffectM (User EffectM)
newUserSubscription _ = gqlEffectResolver ["UPDATE_USER"] fetchUser

createAddressMutation :: a -> EffectM Address
createAddressMutation _ = gqlEffectResolver ["UPDATE_ADDRESS"] (fetchAddress (Euro 1 0))

newAddressSubscription :: a -> EffectM Address
newAddressSubscription _ = gqlEffectResolver ["UPDATE_ADDRESS"] $ fetchAddress (Euro 1 0)

newtype IUnArgs = IUnArgs
  { unionTest :: Maybe InputUnion
  } deriving (Show, Generic)

iUnTestResolver :: IUnArgs -> ResM Text
iUnTestResolver args = return $ pack $ show args

data Query = Query
  { user       :: () -> ResM (User ResM)
  , wrappedA1  :: A (Int, Text)
  , iUnTest    :: IUnArgs -> ResM Text
  , wrappedA2  :: A Text
  , integerSet :: Set Int
  , textIntMap :: Map Text Int
  } deriving (Generic)

data Mutation = Mutation
  { createUser    :: () -> EffectM (User EffectM)
  , createAddress :: () -> EffectM Address
  } deriving (Generic)

data Subscription = Subscription
  { newUser    :: () -> EffectM (User EffectM)
  , newAddress :: () -> EffectM Address
  } deriving (Generic)

gqlRoot :: GQLRootResolver IO Query Mutation Subscription
gqlRoot =
  GQLRootResolver
    { queryResolver =
        return
          Query
            { user = const $ gqlResolver fetchUser
            , wrappedA1 = A (0, "")
            , iUnTest = iUnTestResolver
            , wrappedA2 = A ""
            , integerSet = S.fromList [1, 2]
            , textIntMap = M.fromList [("robin", 1), ("carl", 2)]
            }
    , mutationResolver = return Mutation {createUser = createUserMutation, createAddress = createAddressMutation}
    , subscriptionResolver = return Subscription {newUser = newUserSubscription, newAddress = newAddressSubscription}
    }
