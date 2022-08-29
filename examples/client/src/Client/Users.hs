{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Client.Users
  ( fetchUser,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    Fetch (..),
    FetchError,
    ScalarValue (Int),
    declareGlobalTypes,
    declareLocalTypesInline,
    raw,
  )
import Data.Text (Text)
import Prelude

data Euro
  = Euro
      Int
      Int
  deriving (Show, Eq)

instance DecodeScalar Euro where
  decodeScalar _ = pure (Euro 1 0)

instance EncodeScalar Euro where
  encodeScalar (Euro x y) = Int (x * 101 + y)

declareGlobalTypes "assets/users.gql"

declareLocalTypesInline
  "assets/users.gql"
  [raw|
    # Subscription Test Query
    subscription MySubscription
    {
      newUser
      { subEmail : email }
    }
  |]

declareLocalTypesInline
  "assets/users.gql"
  [raw|
    # Query Hero with Compile time Validation
    query GetUser ($coordinates: Coordinates!)
      {
        myUser: user {
           name
           aliasEmail: email
           address (coordinates: $coordinates ){
             city
           }
           aliasAdress: address (coordinates: $coordinates ){
              city
           }
        }
        user {
          email
          name
        }
      }
  |]

fetchUser :: (ByteString -> IO ByteString) -> IO (Either (FetchError GetUser) GetUser)
fetchUser = flip fetch args
  where
    args :: Args GetUser
    args =
      GetUserArgs
        { coordinates =
            Coordinates
              { longitude = [],
                latitude = Euro 1 2
              }
        }
