{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.Holistic.API
  ( api
  ) where

import           Data.Morpheus          (interpreter)
import           Data.Morpheus.Document (gqlDocument)
import           Data.Morpheus.Kind     (SCALAR)
import           Data.Morpheus.Types    (Event (..), GQLRequest, GQLResponse, GQLRootResolver (..), GQLScalar (..),
                                         GQLType (..), ID (..), IORes, IOSubRes, ScalarValue (..))
import           Data.Text              (Text)
import           GHC.Generics           (Generic)

data TestScalar =
  TestScalar Int
             Int
  deriving (Generic)

instance GQLType TestScalar where
  type KIND TestScalar = SCALAR

instance GQLScalar TestScalar where
  parseValue _ = pure (TestScalar 1 0)
  serialize (TestScalar x y) = Int (x * 100 + y)

[gqlDocument|

enum TestEnum {
   EnumA
   EnumB
   EnumC
}

input NestedInputObject {
  fieldTestID : ID!
}

input TestInputObject {
  fieldTestScalar : TestScalar!
  fieldNestedInputObject : [NestedInputObject]!
}

input Coordinates {
  latitude  : TestScalar!
  longitude : Int!
}

type Address {
  city: Text!
  street(argInputObject: TestInputObject!, argMaybeString: Text): [[[[Text!]!]!]]
  houseNumber : Int!
}

union TestUnion  = User | Address

type User {
  name    : Text!
  email   : Text!
  address (coordinates: Coordinates!, comment: Text  ):  Address!
  office  (zipCode: [Int!] , cityID: TestEnum! ):  Address!
  friend  : User
}

type Query {
  user      :  User!
  testUnion : TestUnion
}

|]

newtype Mutation = Mutation
  { createUser :: AddressArgs -> IORes User
  } deriving (Generic)

data EVENT =
  EVENT
  deriving (Show, Eq)

newtype Subscription = Subscription
  { newUser :: AddressArgs -> IOSubRes EVENT () User
  } deriving (Generic)

resolveValue :: Monad m => b -> a -> m b
resolveValue = const . return

resolveUser :: a -> IORes User
resolveUser _ =
  return $
  User
    { name = resolveValue "testName"
    , email = resolveValue ""
    , address = resolveAddress
    , office = resolveAddress
    , friend = resolveValue Nothing
    }
  where
    resolveAddress _ =
      return Address {city = resolveValue "", houseNumber = resolveValue 0, street = resolveValue Nothing}

rootResolver :: GQLRootResolver IO EVENT () Query Mutation Subscription
rootResolver =
  GQLRootResolver
    { queryResolver = return Query {user = resolveUser, testUnion = const $ return Nothing}
    , mutationResolver = return Mutation {createUser = resolveUser}
    , subscriptionResolver = return Subscription {newUser = const $ Event [EVENT] resolveUser}
    }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
