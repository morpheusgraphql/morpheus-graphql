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
                                                )
import           Example.Files                  ( getJson )
import           Data.Aeson                     ( FromJSON )
import           Data.Either
import           Control.Monad.Trans            ( lift )

data Coord = Coord {
    latitude :: Text,
    longitude :: Text
} deriving (Show,Generic,Data,GQLArgs)

data Zip = Zip {
    zipcode:: Text
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
        ,address:: Coord ::-> Address
        ,office:: Zip ::-> Address
        ,friend:: Maybe User
        ,home :: Maybe Address
} deriving (Show,Generic,Data,GQLSelection , FromJSON )

data Query = Query {
    user:: () ::-> User
} deriving (Show,Generic,Data,GQLRoot, FromJSON )



fetchAddress :: Text -> Text -> EvalIO Address
fetchAddress cityName streetName = lift (getJson "address")
    >>= eitherToResponse modify
  where
    modify address = address { city   = concat [cityName, " ", city address]
                             , street = streetName
                             }

resolveAddress :: Coord ::-> Address
resolveAddress = Resolver resolve
    where resolve args = fetchAddress (latitude args) (longitude args)

resolveOffice :: User -> Zip ::-> Address
resolveOffice user = Resolver resolve
    where resolve args = fetchAddress (zipcode args) "some bla"

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
