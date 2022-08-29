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

module Client.NewUsers
  ( subscribeNewUsers,
  )
where

import Data.Morpheus.Client
  ( ClientStream,
    DecodeScalar (..),
    EncodeScalar (..),
    FetchError,
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
    subscription NewUsers ($loc: Coordinates!)
      {
         newUser {
           name
           email
           address (coordinates: $loc ){
             city
           }
        }
      }
  |]

args :: NewUsersArgs
args = NewUsersArgs {loc = Coordinates {longitude = [], latitude = Euro 1 2}}

printResponse :: Either (FetchError NewUsers) NewUsers -> IO ()
printResponse (Right NewUsers {newUser}) = print newUser
printResponse (Left e) = print e

stream :: IO (ClientStream t NewUsers)
stream = request "ws://localhost:3000" args >>= forEach handler