{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Example.Schema
  ( resolve
  ) where

import           Control.Monad.Trans        (lift)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Data                  (Data)
import           Data.Maybe                 (fromMaybe)
import           Data.Morpheus              ((::->) (..), EnumOf (unpackEnum), GQLResponse, GQLRoot (..), ResolveIO,
                                             ScalarOf (..), eitherToResponse, interpreter)
import           Data.Morpheus.Kind         (GQLArgs, GQLEnum, GQLInput, GQLKind (..), GQLMutation, GQLQuery,
                                             GQLSelection, Scalar (..))
import qualified Data.Morpheus.Types.JSType as S (Scalar (..))
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

newtype Even =
  Even Int
  deriving (Show, Data, Generic, GQLKind)

instance Scalar Even where
  parseValue (S.Int x) = pure $ Even (x * 2)
  parseValue _         = pure $ Even 2
  serialize (Even value) = S.Int value

data Coordinates = Coordinates
  { latitude  :: ScalarOf Even
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
  } deriving (Generic, Show, GQLKind, GQLSelection, Data)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: LocationByCoordinates ::-> Address
  , office  :: Location ::-> Address
  , friend  :: () ::-> Maybe User
  , home    :: Maybe Address
  } deriving (Show, Generic, Data, GQLSelection)

instance GQLKind User where
  description _ = "Custom Description for Client Defined User Type"

newtype Query = Query
  { user :: () ::-> User
  } deriving (Show, Generic, Data, GQLQuery)

newtype Mutation = Mutation
  { createUser :: LocationByCoordinates ::-> User
  } deriving (Show, Generic, Data, GQLMutation)

fetchAddress :: Even -> Text -> ResolveIO Address
fetchAddress (Even x) streetName = lift M.jsonAddress >>= eitherToResponse modify
  where
    modify mAddress =
      Address
        { city = T.concat [pack $ show x, " ", M.city mAddress]
        , houseNumber = M.houseNumber mAddress
        , street = streetName
        , owner = Nothing
        }

resolveAddress :: LocationByCoordinates ::-> Address
resolveAddress = Resolver res
  where
    res args = fetchAddress (unpackScalar $ latitude $ coordinates args) (pack $ show $ longitude $ coordinates args)

addressByCityID :: CityID -> Int -> ResolveIO Address
addressByCityID Paris code = fetchAddress (Even $ 75 + code) "Paris"
addressByCityID BLN code   = fetchAddress (Even $ 10 + code) "Berlin"
addressByCityID HH code    = fetchAddress (Even $ 20 + code) "Hamburg"

resolveOffice :: M.JSONUser -> Location ::-> Address
resolveOffice _ = Resolver resolve'
  where
    resolve' args = addressByCityID (unpackEnum $ cityID args) (fromMaybe 101 (zipCode args))

resolveUser :: () ::-> User
resolveUser = Resolver resolve'
  where
    resolve' _ = lift M.jsonUser >>= eitherToResponse modify
    modify user' =
      User
        { name = M.name user'
        , email = M.email user'
        , address = resolveAddress
        , office = resolveOffice user'
        , home = Nothing
        , friend = Resolver $ \_ -> pure Nothing
        }

createUserMutation :: LocationByCoordinates ::-> User
createUserMutation = Resolver resolve'
  where
    resolve' _ = lift M.jsonUser >>= eitherToResponse modify
    modify user' =
      User
        { name = M.name user'
        , email = M.email user'
        , address = resolveAddress
        , office = resolveOffice user'
        , home = Nothing
        , friend = Resolver $ \_ -> pure Nothing
        }

resolve :: B.ByteString -> IO GQLResponse
resolve =
  interpreter
    GQLRoot {queryResolver = Query {user = resolveUser}, mutationResolver = Mutation {createUser = createUserMutation}}
