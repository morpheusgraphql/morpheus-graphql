{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveDataTypeable, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings  #-}

module Example.Schema ( resolve ) where

import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )
import qualified Data.Text                     as T
                                                ( concat )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.Morpheus                  ( GQLSelection
                                                , GQLQuery
                                                , GQLArgs
                                                , (::->)(..)
                                                , GQLResponse
                                                , GQLRequest
                                                , interpreter
                                                , eitherToResponse
                                                , ResolveIO(..)
                                                , GQLInput
                                                , EnumOf(unpackEnum)
                                                , GQLEnum
                                                , GQLRoot(..)
                                                , NoMutation(..)
                                                , GQLMutation
                                                )
import           Example.Files                  ( getJson )
import           Data.Aeson                     ( FromJSON )
import           Data.Either
import           Control.Monad.Trans            ( lift )
import           Data.Maybe                     ( fromMaybe )

data CityID = Paris | BLN | HH deriving (Show,Generic,Data, GQLEnum)

data Coordinates = Coordinates {
    latitude :: Text,
    longitude :: Text
} deriving (Show,Generic,Data,GQLInput)

data LocationByCoordinates = LocationByCoordinates {
    coordinates :: Coordinates
} deriving (Show,Generic,Data,GQLArgs)

data Location = Location {
    zipCode:: Maybe Int,
    cityID:: EnumOf CityID
} deriving (Show,Data,Generic,GQLArgs)

data Address = Address {
  city :: Text,
  street :: Text,
  houseNumber :: Int,
  owner:: Maybe User
} deriving (Generic, Show, GQLSelection, Data, FromJSON)

data User = User {
  name :: Text,
  email :: Text,
  address:: LocationByCoordinates ::-> Address,
  office:: Location ::-> Address,
  friend:: Maybe User,
  home :: Maybe Address
} deriving (Show,Generic,Data,GQLSelection, FromJSON )

newtype Query = Query {
  user:: () ::-> User
} deriving (Show,Generic,Data, GQLQuery, FromJSON )

newtype Mutation = Mutation {
  createUser:: LocationByCoordinates ::-> User
} deriving (Show,Generic,Data, GQLMutation , FromJSON )

fetchAddress :: Text -> Text -> ResolveIO Address
fetchAddress cityName streetName = lift (getJson "address")
    >>= eitherToResponse modify
  where
    modify address = address { city   = T.concat [cityName, " ", city address]
                             , street = streetName
                             }

resolveAddress :: LocationByCoordinates ::-> Address
resolveAddress = Resolver resolve
  where
    resolve args = fetchAddress (latitude $ coordinates args)
                                (longitude $ coordinates args)

addressByCityID Paris code = fetchAddress (pack $ "75" ++ code) "Paris"
addressByCityID BLN   code = fetchAddress (pack $ "10" ++ code) "Berlin"
addressByCityID HH    code = fetchAddress (pack $ "20" ++ code) "Hamburg"

resolveOffice :: User -> Location ::-> Address
resolveOffice user = Resolver resolve
  where
    resolve args = addressByCityID (unpackEnum $ cityID args)
                                   (show $ fromMaybe 101 (zipCode args))


resolveUser :: () ::-> User
resolveUser = Resolver resolve
  where
    resolve _ = lift (getJson "user") >>= eitherToResponse modify
    modify user =
        user { address = resolveAddress, office = resolveOffice user }

createUserMutation :: LocationByCoordinates ::-> User
createUserMutation = Resolver resolve
  where
    resolve _ = lift (getJson "user") >>= eitherToResponse modify
    modify user =
        user { address = resolveAddress, office = resolveOffice user }

resolve :: GQLRequest -> IO GQLResponse
resolve = interpreter GQLRoot
    {
      queryResolver    = Query {
        user = resolveUser
      },
      mutationResolver = Mutation {
        createUser = createUserMutation
      }
    }
