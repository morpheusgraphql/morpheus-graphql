{-# LANGUAGE DeriveGeneric , DeriveAnyClass , DeriveDataTypeable, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses , OverloadedStrings  #-}

module Example.Schema
    ( gqlHandler
    )
where

import           Prelude                 hiding ( concat )
import           GHC.Generics                   ( Generic )
import           Data.Data                      ( Data )
import           Data.Text                      ( Text
                                                , concat
                                                , pack
                                                , unpack
                                                )
import           Data.Morpheus                  ( GQLSelection
                                                , GQLRoot
                                                , GQLArgs
                                                , (::->)(..)
                                                , GQLResponse
                                                , GQLRequest
                                                , interpreter
                                                , eitherToResponse
                                                , EvalIO(..)
                                                , GQLInput
                                                )
import           Example.Files                  ( getJson )
import           Data.Aeson                     ( FromJSON )
import           Data.Either
import           Control.Monad.Trans            ( lift )

data Coordinates = Coordinates {
    latitude :: Text,
    longitude :: Text
} deriving (Show,Generic,Data,GQLArgs)

data CityID = Paris | Hamburg | Berlin deriving (Show,Generic,Data,GQLInput)

data Location = Location {
    zipCode:: Text,
    cityID:: CityID
} deriving (Show,Data,Generic,GQLArgs)

data Address = Address {
        city :: Text
        ,street :: Text
        ,houseNumber :: Int
        ,owner:: Maybe User
} deriving (Generic,Show,GQLSelection,Data, FromJSON)

data User = User {
        name :: Text
        ,email :: Text
        ,address:: Coordinates ::-> Address
        ,office:: Location ::-> Address
        ,friend:: Maybe User
        ,home :: Maybe Address
} deriving (Show,Generic,Data,GQLSelection , FromJSON )

newtype Query = Query {
    user:: () ::-> User
} deriving (Show,Generic,Data,GQLRoot, FromJSON )

fetchAddress :: Text -> Text -> EvalIO Address
fetchAddress cityName streetName = lift (getJson "address")
    >>= eitherToResponse modify
  where
    modify address = address { city   = concat [cityName, " ", city address]
                             , street = streetName
                             }

resolveAddress :: Coordinates ::-> Address
resolveAddress = Resolver resolve
    where resolve args = fetchAddress (latitude args) (longitude args)

addressByCityID Paris code = fetchAddress (pack $ "75" ++ code) "Paris"
addressByCityID Berlin code = fetchAddress (pack $ "10" ++ code) "Berlin"
addressByCityID Hamburg code = fetchAddress (pack $ "20" ++ code) "Hamburg"

resolveOffice :: User -> Location ::-> Address
resolveOffice user = Resolver resolve
    where resolve args = addressByCityID (cityID args) (unpack $ zipCode args)


resolveUser :: () ::-> User
resolveUser = Resolver resolve
  where
    resolve _ = lift (getJson "user") >>= eitherToResponse modify
    modify user =
        user { address = resolveAddress, office = resolveOffice user }

resolveRoot :: EvalIO Query
resolveRoot = pure $ Query { user = resolveUser }

gqlHandler :: GQLRequest -> IO GQLResponse
gqlHandler = interpreter resolveRoot
