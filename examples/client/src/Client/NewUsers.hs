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
  ( newUsers,
  )
where

import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    GQLClient,
    ResponseStream,
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

newUsers :: GQLClient -> IO (ResponseStream NewUsers)
newUsers = (`request` args)
