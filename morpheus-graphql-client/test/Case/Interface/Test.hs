{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.Interface.Test
  ( testInterface,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
    readFile,
  )
import Data.Morpheus.Client
  ( Fetch (..),
    defineByDocumentFile,
    gql,
  )
import Data.Text (Text)
import Test.Tasty.HUnit
  ( assertEqual,
  )
import Prelude
  ( Either (..),
    IO,
    String,
  )

defineByDocumentFile
  "morpheus-graphql-client/test/Case/Interface/schema.gql"
  [gql|
    query MyQuery {
      character {
        name
        ... on Deity {
              power
        }
        ... on Hero {
              hoby
        }
      }
      character2: character {
        name1: name
        name
      }

      character3: character {
        ... on Hero {
              hoby
        }
        ... on Character {
              name2: name
        }
      }
      character4: character {
        ... on Hero {
              hoby
        }
      }
    }
  |]

resolver :: ByteString -> IO ByteString
resolver _ = readFile "morpheus-graphql-client/test/Case/Interface/response.json"

client :: IO (Either String MyQuery)
client = fetch resolver ()

testInterface :: IO ()
testInterface = do
  value <- client
  assertEqual
    "test interface"
    ( Right
        MyQuery
          { character = [],
            character2 = [],
            character3 = [],
            character4 = []
          }
    )
    value
