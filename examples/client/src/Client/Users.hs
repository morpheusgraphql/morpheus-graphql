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

import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    FetchError,
    Request (..),
    ScalarValue (Int),
    declareGlobalTypes,
    declareLocalTypesInline,
    raw,
    request,
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

fetchUser :: IO (Either (FetchError GetUser) GetUser)
fetchUser =
  request
    "http://localhost:3000"
    ( HttpRequest
        { requestArgs =
            GetUserArgs
              { coordinates =
                  Coordinates
                    { longitude = [],
                      latitude = Euro 1 2
                    }
              }
        }
    )
