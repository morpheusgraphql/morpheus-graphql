{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Subscription.Case.ApolloRequest
  ( testApolloRequest,
    SubM,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus (App)
import Data.Morpheus.Subscriptions
  ( Event,
  )
import Relude hiding (ByteString)
import Subscription.Utils
  ( SimulationState (..),
    SubM,
    apolloConnectionAck,
    apolloConnectionErr,
    apolloInit,
    apolloPing,
    apolloPong,
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
  (Eq ch, Show ch, Hashable ch) =>
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
            ["Error in $.type: Invalid type encountered."]
            outputs,
          storeIsEmpty store
        ]

testConnectionInit ::
  (Eq ch, Show ch, Hashable ch) =>
  App (Event ch a) (SubM (Event ch a)) ->
  IO TestTree
testConnectionInit = testSimulation test [apolloInit]
  where
    test input SimulationState {inputs, outputs, store} =
      testGroup
        "connection init"
        [ inputsAreConsumed inputs,
          testResponse
            [apolloConnectionAck]
            outputs,
          stored input store,
          storedSingle store
        ]

testPingPong ::
  (Eq ch, Show ch, Hashable ch) =>
  App (Event ch a) (SubM (Event ch a)) ->
  IO TestTree
testPingPong = testSimulation test [apolloInit, apolloPing]
  where
    test input SimulationState {inputs, outputs, store} =
      testGroup
        "ping pong"
        [ inputsAreConsumed inputs,
          testResponse
            [apolloConnectionAck, apolloPong]
            outputs
        ]

startSub :: ByteString -> ByteString
startSub = apolloStart "subscription MySubscription { newDeity { name }}"

testSubscriptionStart ::
  (Eq ch, Show ch, Hashable ch) =>
  WSApp ch a ->
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
            [apolloConnectionAck]
            outputs,
          storeSubscriptions
            input
            ["1", "5"]
            store
        ]

testSubscriptionStop ::
  (Eq ch, Show ch, Hashable ch) =>
  WSApp ch a ->
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
            [apolloConnectionAck]
            outputs,
          storeSubscriptions
            input
            ["5"]
            store
        ]

testApolloRequest ::
  (Eq ch, Show ch, Hashable ch) =>
  App (Event ch a) (SubM (Event ch a)) ->
  IO TestTree
testApolloRequest app =
  testGroup "ApolloRequest"
    <$> traverse
      (app &)
      [ testUnknownType,
        testConnectionInit,
        testSubscriptionStart,
        testSubscriptionStop,
        testPingPong
      ]
