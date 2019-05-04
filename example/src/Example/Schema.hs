{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Example.Schema
  ( gqlApi
  ) where

import qualified Data.ByteString.Lazy.Char8  as B
import           Data.Maybe                  (fromMaybe)
import           Data.Morpheus               (interpreter)
import           Data.Morpheus.Kind          (GQLArgs, GQLEnum, GQLInput, GQLKind (..), GQLMutation, GQLObject,
                                              GQLQuery, GQLScalar (..))
import           Data.Morpheus.Kind.Internal (GQL, PRIMITIVE,SCALAR)
import           Data.Morpheus.Types         (ScalarValue (..))
import           Data.Morpheus.Wrapper       ((::->) (..), EnumOf,ScalarOf(..), GQLRoot (..), unwrap)
import           Data.Text                   (Text, pack)
import           Data.Typeable               (Typeable)
import qualified Example.Model               as M (JSONAddress (..), JSONUser (..), jsonAddress, jsonUser)
import           GHC.Generics                (Generic)

data CityID
  = Paris
  | BLN
  | HH
  deriving (Show, Generic, Typeable, GQLEnum)

instance GQLKind CityID where
  description _ = "ID of Cities in Zip Format"

data Seven =
  Seven
  deriving (Typeable, Generic, GQLKind)

instance GQLScalar Seven where
  parseValue _ = pure Seven
  serialize Seven = Int 7

type instance GQL Seven = SCALAR

type instance GQL Text = PRIMITIVE

type instance GQL UID = PRIMITIVE

data UID = UID
  { uid :: Text
  } deriving (Show, Generic, Typeable, GQLKind, GQLInput)

data Coordinates = Coordinates
  { latitude  :: ScalarOf Seven
  , longitude :: UID
  } deriving (Generic, Typeable, GQLInput)

instance GQLKind Coordinates where
  description _ = "just random latitude and longitude"

data LocationByCoordinates = LocationByCoordinates
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Generic, GQLArgs)

data Location = Location
  { zipCode :: Maybe [Int]
  , cityID  :: EnumOf CityID
  } deriving (Generic, GQLArgs)

data Address = Address
  { city        :: Text
  , street      :: Text
  , houseNumber :: Int
  , owner       :: Maybe User
  } deriving (Generic, GQLKind, GQLObject, Typeable)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: LocationByCoordinates ::-> Address
  , office  :: Location ::-> Address
  , friend  :: () ::-> Maybe User
  , home    :: Maybe Address
  } deriving (Generic, Typeable, GQLObject)

instance GQLKind User where
  description _ = "Custom Description for Client Defined User Type"

newtype Query = Query
  { user :: () ::-> User
  } deriving (Generic, GQLQuery)

newtype Mutation = Mutation
  { createUser :: LocationByCoordinates ::-> User
  } deriving (Generic, GQLMutation)

fetchAddress :: Seven -> Text -> IO (Either String Address)
fetchAddress Seven streetName = do
  address' <- M.jsonAddress
  pure (modify <$> address')
  where
    modify mAddress =
      Address {city = M.city mAddress, houseNumber = M.houseNumber mAddress, street = streetName, owner = Nothing}

resolveAddress :: LocationByCoordinates ::-> Address
resolveAddress = Resolver res
  where
    res args = fetchAddress Seven (pack $ show $ longitude $ coordinates args)

addressByCityID :: CityID -> Int -> IO (Either String Address)
addressByCityID Paris code = fetchAddress Seven "Paris"
addressByCityID BLN code   = fetchAddress Seven "Berlin"
addressByCityID HH code    = fetchAddress Seven "Hamburg"

resolveOffice :: M.JSONUser -> Location ::-> Address
resolveOffice _ = Resolver resolve'
  where
    resolve' args = addressByCityID (unwrap $ cityID args) (head $ fromMaybe [101] (zipCode args))

resolveUser :: () ::-> User
resolveUser = Resolver $ const (M.jsonUser >>= \x -> return (buildResolverBy <$> x))
  where
    buildResolverBy user' =
      User
        { name = M.name user'
        , email = M.email user'
        , address = resolveAddress
        , office = resolveOffice user'
        , home = Nothing
        , friend = Resolver $ \_ -> pure (pure Nothing)
        }

createUserMutation :: LocationByCoordinates ::-> User
createUserMutation = Resolver resolve'
  where
    resolve' _ = do
      result <- M.jsonUser
      pure (modify <$> result)
    modify user' =
      User
        { name = M.name user'
        , email = M.email user'
        , address = resolveAddress
        , office = resolveOffice user'
        , home = Nothing
        , friend = Resolver $ \_ -> pure (pure Nothing)
        }

gqlApi :: B.ByteString -> IO B.ByteString
gqlApi = interpreter GQLRoot {query = Query {user = resolveUser}, mutation = Mutation {createUser = createUserMutation}}
