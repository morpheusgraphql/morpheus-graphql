{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Example.Schema
  ( gqlApi
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Maybe                 (fromMaybe)
import           Data.Morpheus              (interpreter)
import           Data.Morpheus.Kind         (ENUM, GQLArgs, GQLMutation, GQLQuery, GQLScalar (..), GQLType (..),
                                             INPUT_OBJECT, KIND, OBJECT, SCALAR)
import           Data.Morpheus.Types        ((::->) (..), GQLRoot (..), ScalarValue (..))
import           Data.Text                  (Text, pack)
import           Example.Model              (JSONAddress, JSONUser, jsonAddress, jsonUser)
import qualified Example.Model              as M (JSONAddress (..), JSONUser (..))
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
  } deriving (Show, Generic, GQLType)

data Coordinates = Coordinates
  { latitude  :: Euro
  , longitude :: [UID]
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

fetchAddress :: Euro -> Text -> IO (Either String Address)
fetchAddress _ streetName = do
  address' <- jsonAddress
  pure (transformAddress streetName <$> address')

transformAddress :: Text -> JSONAddress -> Address
transformAddress street' address' =
  Address {city = M.city address', houseNumber = M.houseNumber address', street = street', owner = Nothing}

resolveAddress :: AddressArgs ::-> Address
resolveAddress = Resolver $ \args -> fetchAddress (Euro 1 0) (pack $ show $ longitude $ coordinates args)

addressByCityID :: CityID -> Int -> IO (Either String Address)
addressByCityID Paris code = fetchAddress (Euro 1 code) "Paris"
addressByCityID BLN code   = fetchAddress (Euro 1 code) "Berlin"
addressByCityID HH code    = fetchAddress (Euro 1 code) "Hamburg"

resolveOffice :: JSONUser -> OfficeArgs ::-> Address
resolveOffice _ = Resolver $ \args -> addressByCityID (cityID args) (head $ fromMaybe [101] (zipCode args))

resolveUser :: () ::-> User
resolveUser = transformUser <$> Resolver (const jsonUser)

transformUser :: JSONUser -> User
transformUser user' =
  User
    { name = M.name user'
    , email = M.email user'
    , address = resolveAddress
    , office = resolveOffice user'
    , home = HH
    , friend = return Nothing
    }

createUserMutation :: AddressArgs ::-> User
createUserMutation = transformUser <$> Resolver (const jsonUser)

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter GQLRoot {query = Query {user = resolveUser}, mutation = Mutation {createUser = createUserMutation}}
