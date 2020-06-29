{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.Case.ApolloRequest
  ( testApolloRequest,
    SubM,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Types
  ( Event,
    Input,
    Stream,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( WS,
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

testUnknownType ::
  (Eq ch) =>
  (Input WS -> Stream WS (Event ch a) (SubM (Event ch a))) ->
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
  (Input WS -> Stream WS (Event ch a) (SubM (Event ch a))) ->
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

testSubscriptionStart ::
  (Eq ch) =>
  (Input WS -> Stream WS (Event ch a) (SubM (Event ch a))) ->
  IO TestTree
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

testSubscriptionStop ::
  (Eq ch) =>
  (Input WS -> Stream WS (Event ch a) (SubM (Event ch a))) ->
  IO TestTree
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
  (Input WS -> Stream WS (Event ch a) (SubM (Event ch a))) ->
  IO TestTree
testApolloRequest api = do
  unknownType <- testUnknownType api
  connection_init <- testConnectionInit api
  start <- testSubscriptionStart api
  stop <- testSubscriptionStop api
  return $ testGroup "ApolloRequest" [unknownType, connection_init, start, stop]
