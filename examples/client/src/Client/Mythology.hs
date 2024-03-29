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

module Client.Mythology
  ( fetchHero,
  )
where

import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    GQLClient,
    ResponseStream,
    ScalarValue (..),
    declareGlobalTypes,
    declareLocalTypesInline,
    raw,
    request,
  )
import Data.Text (Text)

newtype Lifetime
  = Lifetime Int
  deriving (Show, Eq)

instance EncodeScalar Lifetime where
  encodeScalar (Lifetime x) = Int x

instance DecodeScalar Lifetime where
  decodeScalar _ = pure (Lifetime 0)

newtype Power
  = Power Int
  deriving (Show, Eq)

instance EncodeScalar Power where
  encodeScalar (Power x) = Int x

instance DecodeScalar Power where
  decodeScalar _ = pure (Power 1)

declareGlobalTypes "assets/mythology.gql"

declareLocalTypesInline
  "assets/mythology.gql"
  [raw|
    # Query Hero with Compile time Validatio!
    query GetHero ($name:String!, $city: City!)
      {
        deity (name:$name, bornPlace:$city) {
          name
          power
          realm
          bornAt
        }
        character  {
          __typename
          ...on Creature {
            name
            age
          }
          ...on Human {
            name
            bornAt
          }
        }
        persons {
          __typename
          ...on Deity {
              cName: name
          }
          ...on Human {
              humenName: name
              city: bornAt
          }
        }
      }
  |]

fetchHero :: GQLClient -> IO (ResponseStream GetHero)
fetchHero client = request client GetHeroArgs {name = "morpheus", city = CityIthaca}
