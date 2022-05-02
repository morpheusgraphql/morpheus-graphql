{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.JSON.Custom.Mutation
  ( test,
  )
where

import Data.Morpheus.Client
  ( Fetch (..),
    FetchError,
    declareLocalTypesInline,
    raw,
  )
import Relude
import Spec.Utils
  ( mockApi,
    path,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

declareLocalTypesInline
  (path "JSON/Custom/schema.json")
  [raw|
    mutation TestMutation
      {
        mutationTypeName
      }
  |]

client :: IO (Either (FetchError TestMutation) TestMutation)
client = fetch (mockApi "JSON/Custom/Mutation") ()

test :: TestTree
test = testCase "test Mutation" $ do
  value <- client
  assertEqual
    "test custom Mutation"
    ( Right
        ( TestMutation
            { mutationTypeName = Just "TestMutation"
            }
        )
    )
    value
