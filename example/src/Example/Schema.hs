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
import           Data.Morpheus              ((::->) (..), EnumOf (unpackEnum), GQLArgs, GQLEnum,
                                             GQLInput, GQLMutation, GQLQuery, GQLResponse,
                                             GQLRoot (..), GQLSelection, ResolveIO,
                                             eitherToResponse, interpreter)
import           Data.Text                  (Text, pack)
import qualified Data.Text                  as T (concat)
import qualified Example.Model              as M (JSONAddress (..), JSONUser (..), jsonAddress,
                                                  jsonUser)
import           GHC.Generics               (Generic)

data CityID
  = Paris
  | BLN
  | HH
  deriving (Show, Generic, Data, GQLEnum)

data Coordinates = Coordinates
  { latitude  :: Text
  , longitude :: Int
  } deriving (Show, Generic, Data, GQLInput)

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
  } deriving (Generic, Show, GQLSelection, Data)

data User = User
  { name    :: Text
  , email   :: Text
  , address :: LocationByCoordinates ::-> Address
  , office  :: Location ::-> Address
  , friend  :: Maybe User
  , home    :: Maybe Address
  } deriving (Show, Generic, Data, GQLSelection)

newtype Query = Query
  { user :: () ::-> User
  } deriving (Show, Generic, Data, GQLQuery)

newtype Mutation = Mutation
  { createUser :: LocationByCoordinates ::-> User
  } deriving (Show, Generic, Data, GQLMutation)

fetchAddress :: Text -> Text -> ResolveIO Address
fetchAddress cityName streetName = lift M.jsonAddress >>= eitherToResponse modify
  where
    modify mAddress =
      Address
        { city = T.concat [cityName, " ", M.city mAddress]
        , houseNumber = M.houseNumber mAddress
        , street = streetName
        , owner = Nothing
        }

resolveAddress :: LocationByCoordinates ::-> Address
resolveAddress = Resolver res
  where
    res args = fetchAddress (latitude $ coordinates args) (pack $ show $ longitude $ coordinates args)

addressByCityID :: CityID -> String -> ResolveIO Address
addressByCityID Paris code = fetchAddress (pack $ "75" ++ code) "Paris"
addressByCityID BLN code   = fetchAddress (pack $ "10" ++ code) "Berlin"
addressByCityID HH code    = fetchAddress (pack $ "20" ++ code) "Hamburg"

resolveOffice :: M.JSONUser -> Location ::-> Address
resolveOffice _ = Resolver resolve'
  where
    resolve' args = addressByCityID (unpackEnum $ cityID args) (show $ fromMaybe 101 (zipCode args))

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
        , friend = Nothing
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
        , friend = Nothing
        }

resolve :: B.ByteString -> IO GQLResponse
resolve =
  interpreter
    GQLRoot {queryResolver = Query {user = resolveUser}, mutationResolver = Mutation {createUser = createUserMutation}}
