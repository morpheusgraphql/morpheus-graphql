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

module Client.Client
  ( fetchUser,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Client
  ( Fetch (..),
    defineByDocumentFile,
    gql,
  )
import Data.Morpheus.Types
  ( ScalarDecoder (..),
    ScalarEncoder (..),
    ScalarValue (..),
  )
import Data.Text (Text)
import Prelude

data Euro
  = Euro
      Int
      Int
  deriving (Show, Eq)

instance ScalarDecoder Euro where
  parseValue _ = pure (Euro 1 0)

instance ScalarEncoder Euro where
  serialize (Euro x y) = Int (x * 101 + y)

defineByDocumentFile
  "src/Server/Sophisticated/api.gql"
  [gql|
    # Subscription Test Query
    subscription MySubscription
    {
      newUser
      { subEmail : email }
    }
  |]

defineByDocumentFile
  "src/Server/Sophisticated/api.gql"
  [gql|
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

fetchUser :: (ByteString -> IO ByteString) -> IO (Either String GetUser)
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
