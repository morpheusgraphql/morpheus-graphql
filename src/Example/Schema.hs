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
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Example.Files                  ( getJson )
import           Data.Aeson                     ( FromJSON )
import           Data.Either

data AddressArg = AddressArg {
    token:: Text,
    cityID:: Text
} deriving (Show,Generic,Data,GQLArgs)

data OfficeArg = OfficeArg {
    officeID:: Text
} deriving (Show,Data,Generic,GQLArgs)

data Address = Address {
        city :: Text
        ,street :: Text
        ,houseNumber :: Int
        ,owner:: Maybe User
} deriving (Generic,Show,GQLRecord,Data, FromJSON)

data User = User {
        name :: Text
        ,email :: Text
        ,address:: AddressArg ::-> Address
        ,office:: OfficeArg ::-> Address
        ,friend:: Maybe User
        ,home :: Maybe Address
} deriving (Show,Generic,Data,GQLRecord , FromJSON )

data Query = Query {
    user:: () ::-> User
} deriving (Show,Generic,Data,GQLRoot, FromJSON )

fetchAddress :: Text -> Text -> IO (Eval Address)
fetchAddress cityName streetName = getJson "address"
    >>= eitherToResponce modify
  where
    modify address =
        address { city = concat [cityName, city address], street = streetName }

resolveAddress :: AddressArg ::-> Address
resolveAddress = Resolver (\x -> fetchAddress (token x) (cityID x))

officeResolver :: OfficeArg -> IO (Eval Address)
officeResolver args = fetchAddress (officeID args) "some bla"

userResolver :: () -> IO (Eval User)
userResolver _ = getJson "user" >>= eitherToResponce modify
  where
    modify user =
        user { address = resolveAddress, office = Resolver officeResolver }

rootResolver :: IO (Eval Query)
rootResolver = pure $ pure $ Query { user = Resolver userResolver }

gqlHandler :: GQLRequest -> IO GQLResponce
gqlHandler = interpreter (Proxy :: Proxy Query) rootResolver
