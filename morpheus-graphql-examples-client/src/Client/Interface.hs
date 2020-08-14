{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Client.Interface
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
import Prelude
  ( Either (..),
    IO,
    String,
  )

defineByDocumentFile
  "assets/interface.gql"
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
resolver _ = readFile "morpheus-graphql-examples-client/assets/interfaceRes.json"

testInterface :: IO (Either String MyQuery)
testInterface = fetch resolver ()
