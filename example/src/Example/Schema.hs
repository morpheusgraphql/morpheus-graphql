{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Example.Schema
  ( gqlApi
  ) where

import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Data                  (Data)
import           Data.Maybe                 (fromMaybe)
import           Data.Morpheus              ((::->) (..), EnumOf (unpackEnum), GQLResponse, GQLRoot (..), ScalarOf (..),
                                             interpreter)
import           Data.Morpheus.Kind         (GQLArgs, GQLEnum, GQLInput, GQLKind (..), GQLMutation, GQLObject, GQLQuery,
                                             GQLScalar (..))
import           Data.Morpheus.Types.JSType (ScalarValue (..))
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as T (concat)
import qualified Example.Model              as M (JSONAddress (..), JSONUser (..), jsonAddress, jsonUser)
import           GHC.Generics               (Generic)

data CityID
  = Paris
  | BLN
  | HH
  deriving (Show, Generic, Data, GQLEnum)

instance GQLKind CityID where
  description _ = "ID of Cities in Zip Format"

data Modulo7 =
  Modulo7 Int
          Int
  deriving (Show, Data, Generic, GQLKind)

instance GQLScalar Modulo7 where
  parseValue (Int x) = pure $ Modulo7 (x `div` 7) (x `mod` 7)
  parseValue _       = pure $ Modulo7 0 0
  serialize (Modulo7 value _) = Int value

data Coordinates = Coordinates
  { latitude  :: ScalarOf Modulo7
  , longitude :: Int
  } deriving (Show, Generic, Data, GQLInput)

instance GQLKind Coordinates where
  description _ = "just random latitude and longitude"

data LocationByCoordinates = LocationByCoordinates
  { coordinates :: Coordinates
  , comment     :: Maybe Text
  } deriving (Show, Generic, Data, GQLArgs)

data Location = Location
  { zipCode :: Maybe Int
  , cityID  :: EnumOf CityID
  } deriving (Show, Data, Generic, GQLArgs)

data Address = Address
  { city        :: Text
  , street      :: Text
  , houseNumber :: Int
  , owner       :: Maybe User
  } deriving (Generic, Show, GQLKind, GQLObject, Data)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: LocationByCoordinates ::-> Address
  , office  :: Location ::-> Address
  , friend  :: () ::-> Maybe User
  , home    :: Maybe Address
  } deriving (Show, Generic, Data, GQLObject)

instance GQLKind User where
  description _ = "Custom Description for Client Defined User Type"

newtype Query = Query
  { user :: () ::-> User
  } deriving (Show, Generic, Data, GQLQuery)

newtype Mutation = Mutation
  { createUser :: LocationByCoordinates ::-> User
  } deriving (Show, Generic, Data, GQLMutation)

fetchAddress :: Modulo7 -> Text -> IO (Either String Address)
fetchAddress (Modulo7 x y) streetName = do
  address' <- M.jsonAddress
  pure (modify <$> address')
  where
    modify mAddress =
      Address
        { city = T.concat [pack $ show x, pack $ show y, " ", M.city mAddress]
        , houseNumber = M.houseNumber mAddress
        , street = streetName
        , owner = Nothing
        }

resolveAddress :: LocationByCoordinates ::-> Address
resolveAddress = Resolver res
  where
    res args = fetchAddress (unpackScalar $ latitude $ coordinates args) (pack $ show $ longitude $ coordinates args)

addressByCityID :: CityID -> Int -> IO (Either String Address)
addressByCityID Paris code = fetchAddress (Modulo7 75 code) "Paris"
addressByCityID BLN code   = fetchAddress (Modulo7 10 code) "Berlin"
addressByCityID HH code    = fetchAddress (Modulo7 20 code) "Hamburg"

resolveOffice :: M.JSONUser -> Location ::-> Address
resolveOffice _ = Resolver resolve'
  where
    resolve' args = addressByCityID (unpackEnum $ cityID args) (fromMaybe 101 (zipCode args))

resolveUser :: () ::-> User
resolveUser = Resolver $ const (M.jsonUser >>= \x -> return (modify <$> x))
  where
    modify user' =
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

gqlApi :: B.ByteString -> IO GQLResponse
gqlApi =
  interpreter
    GQLRoot {queryResolver = Query {user = resolveUser}, mutationResolver = Mutation {createUser = createUserMutation}}
