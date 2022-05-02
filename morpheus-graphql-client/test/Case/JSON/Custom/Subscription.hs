{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Case.JSON.Custom.Subscription
  ( test,
  )
where

import Data.Morpheus.Client
  ( Fetch (..),
    FetchError,
    raw,
  )
import Relude
import Spec.Utils
  ( defineClientWithJSON,
    mockApi,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    testCase,
  )

defineClientWithJSON
  "JSON/Custom"
  [raw|
    subscription TestSubscription
      {
        subscriptionTypeName
      }
  |]

client :: IO (Either (FetchError TestSubscription) TestSubscription)
client = fetch (mockApi "JSON/Custom/Subscription") ()

test :: TestTree
test = testCase "test Subscription" $ do
  value <- client
  assertEqual
    "test custom Subscription"
    ( Right
        ( TestSubscription
            { subscriptionTypeName =
                Just "TestSubscription"
            }
        )
    )
    value
