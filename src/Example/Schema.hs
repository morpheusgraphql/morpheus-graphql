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
import           Data.MorpheusGraphQL           ( GQLRecord
                                                , GQLRoot
                                                , GQLArgs
                                                , (::->)(..)
                                                , GQLResponce
                                                , GQLRequest
                                                , interpreter
                                                , eitherToResponce
                                                , Eval(..)
                                                , EvalIO(..)
                                                , liftIO
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Example.Files                  ( getJson )
import           Data.Aeson                     ( FromJSON )
import           Data.Either

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
       -- ,owner:: Maybe User
} deriving (Generic,Show,GQLRecord,Data, FromJSON)

data User = User {
        name :: Text
        ,email :: Text
        ,address:: Coord ::-> Address
        ,office:: Zip ::-> Address
      --  ,friend:: Maybe User
        ,home :: Maybe Address
} deriving (Show,Generic,Data,GQLRecord , FromJSON )

data Query = Query {
    user:: () ::-> User
} deriving (Show,Generic,Data,GQLRoot, FromJSON )



fetchAddress :: Text -> Text -> EvalIO Address
fetchAddress cityName streetName = liftIO (getJson "address")
    >>= eitherToResponce modify
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
    resolve _ = liftIO (getJson "user") >>= eitherToResponce modify
    modify user =
        user { address = resolveAddress, office = resolveOffice user }

resolveRoot :: IO (Eval Query)
resolveRoot = pure $ pure $ Query { user = resolveUser }

gqlHandler :: GQLRequest -> IO GQLResponce
gqlHandler = interpreter (Proxy :: Proxy Query) resolveRoot
