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
import           Data.GraphqlHS.GQLHS           ( GQLRecord
                                                , GQLRoot
                                                , GQLArgs
                                                , Resolver(resolve)
                                                , (::->)(..)
                                                , GQLResponce
                                                , GQLRequest
                                                , interpreter
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
    user:: User
} deriving (Show,Generic,Data,GQLRoot, FromJSON )

instance Resolver AddressArg Address  where
    resolve args _ = getAddress (cityID args) (token args)

instance Resolver OfficeArg Address  where
    resolve args _ = getAddress (officeID args)  ""

getAddress :: Text -> Text -> IO Address
getAddress cityName streetName = do
    address <- getJson "address" >>= pure . fromRight emptyAdress
    pure $ address { city   = concat [cityName, city address]
                   , street = streetName
                   }

emptyUser = User "" "" None None Nothing Nothing
emptyAdress = Address "" "" 0 Nothing

rootValue = getJson "user" >>= pure . Query . fromRight emptyUser

gqlHandler :: GQLRequest -> IO GQLResponce
gqlHandler v = do
    x <- rootValue
    interpreter (Proxy :: Proxy Query) x v
