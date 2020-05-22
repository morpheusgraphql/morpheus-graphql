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
    defineByIntrospectionFile,
    gql,
  )
import Data.Morpheus.Types (GQLScalar (..), ScalarValue (..))
import Data.Text (Text)

data Euro
  = Euro
      Int
      Int
  deriving (Show)

instance GQLScalar Euro where
  parseValue _ = pure (Euro 1 0)
  serialize (Euro x y) = Int (x * 100 + y)

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
        }
      }
  |]

fetchUser :: (ByteString -> IO ByteString) -> IO (Either String GetUser)
fetchUser = flip fetch args
  where
    args :: Args GetUser
    args =
      GetUserArgs
        { getUserArgsCoordinates =
            Coordinates
              { coordinatesLongitude = [],
                coordinatesLatitude = Euro 1 2
              }
        }
