{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.LowercaseTypeName.Test
  ( testLowercaseTypeName,
  )
where

import Control.Applicative (pure)
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.Morpheus.Client
  ( Fetch (..),
    ScalarDeserializer (..),
    ScalarSerializer (..),
    ScalarValue (..),
    gql,
  )
import Data.Text (Text)
import Spec.Utils
  ( defineClientWith,
    mockApi,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )
import Prelude
  ( ($),
    (.),
    Either (..),
    Eq,
    IO,
    Show,
    String,
  )

newtype Uuid = Uuid
  { uuid :: Text
  }
  deriving (Show, Eq)

instance ScalarSerializer Uuid where
  serialize = String . uuid

instance ScalarDeserializer Uuid where
  parseValue (String x) = pure (Uuid x)
  parseValue _ = Left "not valid uid"

defineClientWith
  "LowercaseTypeName"
  [gql|
    query MyQuery {
      user(id: "11343135") {
        id
      }
    }
  |]

resolver :: ByteString -> IO ByteString
resolver = mockApi "LowercaseTypeName"

client :: IO (Either String MyQuery)
client = fetch resolver ()

testLowercaseTypeName :: TestTree
testLowercaseTypeName = testCase "test lowercase type names" $ do
  value <- client
  assertEqual
    "test interface"
    ( Right
        ( MyQuery
            { user =
                UserUser
                  { id = Uuid "11343135"
                  }
            }
        )
    )
    value
