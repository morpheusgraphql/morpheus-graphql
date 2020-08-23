{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.Case.ApolloRequest
  ( testApolloRequest,
    SubM,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Function ((&))
import Data.Morpheus (App)
import Data.Morpheus.Types
  ( Event,
  )
import Subscription.Utils
  ( SimulationState (..),
    SubM,
    apolloInit,
    apolloStart,
    apolloStop,
    inputsAreConsumed,
    storeIsEmpty,
    storeSubscriptions,
    stored,
    storedSingle,
    testResponse,
    testSimulation,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )

type WSApp ch a = App (Event ch a) (SubM (Event ch a))

testUnknownType ::
  (Eq ch) =>
  App (Event ch a) (SubM (Event ch a)) ->
  IO TestTree
testUnknownType =
  testSimulation
    test
    ["{ \"type\":\"bla\" }"]
  where
    test _ SimulationState {inputs, outputs, store} =
      testGroup
        "unknown request type"
        [ inputsAreConsumed inputs,
          testResponse
            ["Unknown Request type \"bla\"."]
            outputs,
          storeIsEmpty store
        ]

testConnectionInit ::
  (Eq ch) =>
  App (Event ch a) (SubM (Event ch a)) ->
  IO TestTree
testConnectionInit = testSimulation test [apolloInit]
  where
    test input SimulationState {inputs, outputs, store} =
      testGroup
        "connection init"
        [ inputsAreConsumed inputs,
          testResponse
            []
            outputs,
          stored input store,
          storedSingle store
        ]

startSub :: ByteString -> ByteString
startSub = apolloStart "subscription MySubscription { newDeity { name }}"

testSubscriptionStart :: (Eq ch) => WSApp ch a -> IO TestTree
testSubscriptionStart =
  testSimulation
    test
    [ apolloInit,
      startSub "1",
      startSub "5"
    ]
  where
    test input SimulationState {inputs, outputs, store} =
      testGroup
        "subscription start"
        [ inputsAreConsumed inputs,
          testResponse
            []
            outputs,
          storeSubscriptions
            input
            ["1", "5"]
            store
        ]

testSubscriptionStop :: (Eq ch) => WSApp ch a -> IO TestTree
testSubscriptionStop =
  testSimulation
    test
    [ apolloInit,
      startSub "1",
      startSub "5",
      apolloStop "1"
    ]
  where
    test input SimulationState {inputs, outputs, store} =
      testGroup
        "stop subscription"
        [ inputsAreConsumed inputs,
          testResponse
            []
            outputs,
          storeSubscriptions
            input
            ["5"]
            store
        ]

testApolloRequest ::
  (Eq ch) =>
  App (Event ch a) (SubM (Event ch a)) ->
  IO TestTree
testApolloRequest app =
  testGroup "ApolloRequest"
    <$> traverse
      (app &)
      [ testUnknownType,
        testConnectionInit,
        testSubscriptionStart,
        testSubscriptionStop
      ]
