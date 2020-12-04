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

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    Fetch (..),
    ID (..),
    ScalarValue (..),
    defineByDocumentFile,
    gql,
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

defineByDocumentFile
  "assets/mythology.gql"
  [gql|
    # Query Hero with Compile time Validatio!
    query GetHero ($god: Realm, $someID: ID!)
      {
        deity (mythology:$god) {
          power
          fullName
        }
        character(characterID: $someID ) {
          ...on Creature {
            name
            immortality
          }
          ...on Human {
            lifetime
            profession
          }
        }
        char2: character(characterID: $someID ) {
          ...on Creature {
              cName: name
          }
          ...on Human {
              lTime: lifetime
              prof: profession
          }
        }
      }
  |]

mythologyApi :: ByteString -> IO ByteString
mythologyApi req = do
  print req
  putStrLn ""
  return
    "{\"data\":{\"deity\":{ \"fullName\": \"name\" }, \"character\":{ \"__typename\":\"Human\", \"lifetime\": \"Lifetime\", \"profession\": \"Artist\" } ,  \"char2\":{ \"__typename\":\"Human\", \"lTime\": \"time\", \"prof\": \"Artist\" }  }}"

fetchHero :: IO (Either String GetHero)
fetchHero =
  fetch
    mythologyApi
    GetHeroArgs
      { god =
          Just
            Realm
              { owner = "Zeus",
                age = Just 10,
                realm = Nothing,
                profession = Just ProfessionArtist
              },
        someID = ID "Hercules"
      }
