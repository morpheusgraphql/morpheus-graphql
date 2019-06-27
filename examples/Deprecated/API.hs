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
import           Data.Morpheus.Types (EffectT, GQLMutation, GQLQuery, GQLRootResolver (..), GQLScalar (..),
                                      GQLSubscription, GQLType (..), ID, Resolver (..), ScalarValue (..), addEffect,
                                      withEffect)
import           Data.Text           (Text)
import           Data.Typeable       (Typeable)
import           Deprecated.Model    (JSONAddress, jsonAddress)
import qualified Deprecated.Model    as M (JSONAddress (..))
import           GHC.Generics        (Generic)

type instance KIND CityID = ENUM

type instance KIND Euro = SCALAR

type instance KIND UID = INPUT_OBJECT

type instance KIND Coordinates = INPUT_OBJECT

type instance KIND Address = OBJECT

type instance KIND (User a) = OBJECT

type instance KIND (MyUnion m) = UNION

type WSResM = (EffectT IO Text)

type QueryResM = IO

data MyUnion m
  = USER (User m)
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
  , address :: Resolver m AddressArgs Address
  , office  :: Resolver m OfficeArgs Address
  , myUnion :: Resolver m () (MyUnion m)
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
  { user      :: Resolver QueryResM () (User QueryResM)
  , wrappedA1 :: A Int
  , wrappedA2 :: A Text
  } deriving (Generic, GQLQuery)

fetchAddress :: Monad m => Euro -> m (Either String Address)
fetchAddress _ = return $ Right $ Address " " "" 0

transformAddress :: Text -> JSONAddress -> Address
transformAddress street' address' =
  Address {city = M.city address', houseNumber = M.houseNumber address', street = street'}

resolveAddress :: Monad m => Resolver m AddressArgs Address
resolveAddress = Resolver $ \_ -> fetchAddress (Euro 1 0)

addressByCityID :: Monad m => CityID -> Int -> m (Either String Address)
addressByCityID Paris code = fetchAddress (Euro 1 code)
addressByCityID BLN code   = fetchAddress (Euro 1 code)
addressByCityID HH code    = fetchAddress (Euro 1 code)

resolveOffice :: Monad m => Resolver m OfficeArgs Address
resolveOffice = Resolver $ \args -> addressByCityID (cityID args) 12

-- :: Resolver QueryResM () (User QueryResM)
-- resolveUser = transformUser <$> Resolver (const jsonUser)
fetchUser :: Monad m => m (Either String (User m))
fetchUser =
  return $
  Right $
  User
    { name = "name"
    , email = "name@email.com"
    , address = resolveAddress
    , office = resolveOffice
    , home = HH
    , myUnion =
        return $
        USER
          (User
             "unionUserName"
             "unionUserMail"
             resolveAddress
             resolveOffice
             (return $ ADDRESS (Address "unionAdressStreet" "unionAdresser" 1))
             HH)
    }

createUserMutation :: Resolver WSResM () (User WSResM)
createUserMutation = Resolver (const $ addEffect ["UPDATE_USER"] fetchUser)

newUserSubscription :: Resolver WSResM () (User WSResM)
newUserSubscription = Resolver (const $ addEffect ["UPDATE_USER"] fetchUser)

createAddressMutation :: Resolver WSResM () Address
createAddressMutation =
  transformAddress "from Mutation" <$> Resolver (const $ withEffect ["UPDATE_ADDRESS"] jsonAddress)

newAddressSubscription :: Resolver WSResM () Address
newAddressSubscription =
  transformAddress "from Subscription" <$> Resolver (const $ withEffect ["UPDATE_ADDRESS"] jsonAddress)

data Mutation = Mutation
  { createUser    :: Resolver WSResM () (User WSResM)
  , createAddress :: Resolver WSResM () Address
  } deriving (Generic, GQLMutation)

data Subscription = Subscription
  { newUser    :: Resolver WSResM () (User WSResM)
  , newAddress :: Resolver WSResM () Address
  } deriving (Generic, GQLSubscription)

gqlRoot :: GQLRootResolver Query Mutation Subscription
gqlRoot =
  GQLRootResolver
    { queryResolver = Query {user = Resolver $ return fetchUser, wrappedA1 = A 0, wrappedA2 = A ""}
    , mutationResolver = Mutation {createUser = createUserMutation, createAddress = createAddressMutation}
    , subscriptionResolver = Subscription {newUser = newUserSubscription, newAddress = newAddressSubscription}
    }
