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
  ( test,
  )
where

import Data.Morpheus.Client
  ( DecodeScalar (..),
    EncodeScalar (..),
    ScalarValue (..),
    declareGlobalTypes,
    declareLocalTypesInline,
    raw,
  )
import Relude
import Spec.Utils
  ( assertFetch,
    path,
  )
import Test.Tasty
  ( TestTree,
  )

newtype Uuid = Uuid
  { uuid :: Text
  }
  deriving (Show, Eq)

instance EncodeScalar Uuid where
  encodeScalar = String . uuid

instance DecodeScalar Uuid where
  decodeScalar (String x) = pure (Uuid x)
  decodeScalar _ = Left "not valid uid"

declareGlobalTypes (path "LowercaseTypeName/schema.gql")

declareLocalTypesInline
  (path "LowercaseTypeName/schema.gql")
  [raw|
    query MyQuery {
      user(id: "11343135") {
        id
      }
    }
  |]

test :: TestTree
test =
  assertFetch
    "LowercaseTypeName"
    Nothing
    ()
    ( Right
        MyQuery
          { user =
              MyQueryUserUser
                { id = Uuid "11343135"
                }
          }
    )
