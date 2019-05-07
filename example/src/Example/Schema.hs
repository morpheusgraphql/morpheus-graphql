{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Example.Schema
  ( gqlApi
  ) where

import           Data.ByteString.Lazy.Char8  (ByteString)
import           Data.Maybe                  (fromMaybe)
import           Data.Morpheus               (interpreter)
import           Data.Morpheus.Kind          (GQLArgs, GQLKind (..), GQLMutation, GQLQuery, GQLScalar (..))
import           Data.Morpheus.Kind.Internal (ENUM, GQL, INPUT_OBJECT, OBJECT, SCALAR)
import           Data.Morpheus.Types         ((::->) (..), GQLRoot (..), ScalarValue (..))
import           Data.Text                   (Text, pack)
import           Example.Model               (JSONUser, jsonAddress, jsonUser)
import qualified Example.Model               as M (JSONAddress (..), JSONUser (..))
import           GHC.Generics                (Generic)

type instance GQL CityID = ENUM

type instance GQL Euro = SCALAR

type instance GQL UID = INPUT_OBJECT

type instance GQL Coordinates = INPUT_OBJECT

type instance GQL Address = OBJECT

type instance GQL User = OBJECT

data CityID
  = Paris
  | BLN
  | HH
  deriving (Generic, GQLKind)

data Euro =
  Euro Int
       Int
  deriving (Generic, GQLKind)

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

newtype UID = UID
  { uid :: Text
  } deriving (Show, Generic, GQLKind)

data Coordinates = Coordinates
  { latitude  :: Euro
  , longitude :: [UID]
  } deriving (Generic)

instance GQLKind Coordinates where
  description _ = "just random latitude and longitude"

data LocationByCoordinates = LocationByCoordinates
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic, GQLArgs)

data Location = Location
  { zipCode :: Maybe [Int]
  , cityID  :: CityID
  } deriving (Generic, GQLArgs)

data Address = Address
  { city        :: Text
  , street      :: Text
  , houseNumber :: Int
  , owner       :: Maybe User
  } deriving (Generic, GQLKind)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: LocationByCoordinates ::-> Address
  , office  :: Location ::-> Address
  , friend  :: () ::-> Maybe User
  , home    :: CityID
  } deriving (Generic)

instance GQLKind User where
  description _ = "Custom Description for Client Defined User Type"

newtype Query = Query
  { user :: () ::-> User
  } deriving (Generic, GQLQuery)

newtype Mutation = Mutation
  { createUser :: LocationByCoordinates ::-> User
  } deriving (Generic, GQLMutation)

fetchAddress :: Euro -> Text -> IO (Either String Address)
fetchAddress _ streetName = do
  address' <- jsonAddress
  pure (modify <$> address')
  where
    modify mAddress =
      Address {city = M.city mAddress, houseNumber = M.houseNumber mAddress, street = streetName, owner = Nothing}

resolveAddress :: LocationByCoordinates ::-> Address
resolveAddress = Resolver $ \args -> fetchAddress (Euro 1 0) (pack $ show $ longitude $ coordinates args)

addressByCityID :: CityID -> Int -> IO (Either String Address)
addressByCityID Paris code = fetchAddress (Euro 1 code) "Paris"
addressByCityID BLN code   = fetchAddress (Euro 1 code) "Berlin"
addressByCityID HH code    = fetchAddress (Euro 1 code) "Hamburg"

resolveOffice :: JSONUser -> Location ::-> Address
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

createUserMutation :: LocationByCoordinates ::-> User
createUserMutation = transformUser <$> Resolver (const jsonUser)

gqlApi :: ByteString -> IO ByteString
gqlApi = interpreter GQLRoot {query = Query {user = resolveUser}, mutation = Mutation {createUser = createUserMutation}}
