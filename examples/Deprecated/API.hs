{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Deprecated.API
  ( gqlRoot
  ) where

import           Data.Morpheus.Kind  (ENUM, GQLArgs, GQLMutation, GQLQuery, GQLScalar (..), GQLSubscription,
                                      GQLType (..), INPUT_OBJECT, KIND, OBJECT, SCALAR, UNION)
import           Data.Morpheus.Types ((::->), GQLRoot (..), ID, Resolver (..), ScalarValue (..))
import           Data.Text           (Text, pack)
import           Deprecated.Model    (JSONAddress, JSONUser, jsonAddress, jsonUser)
import qualified Deprecated.Model    as M (JSONAddress (..), JSONUser (..))
import           GHC.Generics        (Generic)

type instance KIND CityID = ENUM

type instance KIND Euro = SCALAR

type instance KIND UID = INPUT_OBJECT

type instance KIND Coordinates = INPUT_OBJECT

type instance KIND Address = OBJECT

type instance KIND User = OBJECT

type instance KIND MyUnion = UNION

data MyUnion
  = USER User
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
  , owner       :: Maybe User
  } deriving (Generic, GQLType)

data AddressArgs = AddressArgs
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic, GQLArgs)

data OfficeArgs = OfficeArgs
  { zipCode :: Maybe [[Maybe [ID]]]
  , cityID  :: CityID
  } deriving (Generic, GQLArgs)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: AddressArgs ::-> Address
  , office  :: OfficeArgs ::-> Address
  , myUnion :: () ::-> MyUnion
  , home    :: CityID
  } deriving (Generic)

instance GQLType User where
  description _ = "Custom Description for Client Defined User Type"

newtype Query = Query
  { user :: () ::-> User
  } deriving (Generic, GQLQuery)

fetchAddress :: Euro -> Text -> IO (Either String Address)
fetchAddress _ streetName = do
  address' <- jsonAddress
  pure (transformAddress streetName <$> address')

transformAddress :: Text -> JSONAddress -> Address
transformAddress street' address' =
  Address {city = M.city address', houseNumber = M.houseNumber address', street = street', owner = Nothing}

resolveAddress :: AddressArgs ::-> Address
resolveAddress =
  Resolver $ \args -> withEffects [] <$> fetchAddress (Euro 1 0) (pack $ show $ longitude $ coordinates args)

addressByCityID :: CityID -> Int -> IO (Either String Address)
addressByCityID Paris code = fetchAddress (Euro 1 code) "Paris"
addressByCityID BLN code   = fetchAddress (Euro 1 code) "Berlin"
addressByCityID HH code    = fetchAddress (Euro 1 code) "Hamburg"

withEffects :: [k] -> Either String a -> Either String (a, [k])
withEffects channels x = (, channels) <$> x

resolveOffice :: JSONUser -> OfficeArgs ::-> Address
resolveOffice _ = Resolver $ \args -> withEffects [] <$> addressByCityID (cityID args) 12

resolveUser :: () ::-> User
resolveUser = transformUser <$> Resolver (const $ withEffects [] <$> jsonUser)

transformUser :: JSONUser -> User
transformUser user' =
  User
    { name = M.name user'
    , email = M.email user'
    , address = resolveAddress
    , office = resolveOffice user'
    , home = HH
    , myUnion =
        return $
        USER
          (User
             "unionUserName"
             "unionUserMail"
             resolveAddress
             (resolveOffice user')
             (return $ ADDRESS (Address "unionAdressStreet" "unionAdresser" 1 Nothing))
             HH)
    }

createUserMutation :: () ::-> User
createUserMutation = transformUser <$> Resolver (const $ withEffects ["UPDATE_USER"] <$> jsonUser)

newUserSubscription :: () ::-> User
newUserSubscription = transformUser <$> Resolver (const $ withEffects ["UPDATE_USER"] <$> jsonUser)

createAddressMutation :: () ::-> Address
createAddressMutation =
  transformAddress "from Mutation" <$> Resolver (const $ withEffects ["UPDATE_ADDRESS"] <$> jsonAddress)

newAddressSubscription :: () ::-> Address
newAddressSubscription =
  transformAddress "from Subscription" <$> Resolver (const $ withEffects ["UPDATE_ADDRESS"] <$> jsonAddress)

data Mutation = Mutation
  { createUser    :: () ::-> User
  , createAddress :: () ::-> Address
  } deriving (Generic, GQLMutation)

data Subscription = Subscription
  { newUser    :: () ::-> User
  , newAddress :: () ::-> Address
  } deriving (Generic, GQLSubscription)

{-
  data Channels = UserAdded (Async User) | AddressAdded (Async Address)

  data Subscription = Subscription {
      newUser :: Async User,
      newAddress :: Async Address
  }

  -- resolveNewUserSubscription :: Async User
  -- resolveSubscription = async (UserAdded Pending)

  async :: Channels -> Stream Channels
  async = ....

  newtype Async a = Pending | Response { unpackAwait :: Stream Channels } |
-}
gqlRoot :: GQLRoot Query Mutation Subscription
gqlRoot =
  GQLRoot
    { query = Query {user = resolveUser}
    , mutation = Mutation {createUser = createUserMutation, createAddress = createAddressMutation}
    , subscription = Subscription {newUser = newUserSubscription, newAddress = newAddressSubscription}
    }
