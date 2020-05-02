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
  ( Input,
    Stream,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( GQLChannel (..),
    WS,
    connect,
    empty,
  )
import Subscription.Utils
  ( SimulationState (..),
    SubM,
    apolloInit,
    apolloStart,
    apolloStop,
    inputsAreConsumed,
    simulate,
    storeIsEmpty,
    storeSubscriptions,
    stored,
    storedSingle,
    testResponse,
  )
import Test.Tasty
  ( TestTree,
    testGroup,
  )

testUnknownType ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testUnknownType api = do
  input <- connect
  SimulationState {inputs, outputs, store} <- simulate api input (SimulationState ["{ \"type\":\"bla\" }"] [] empty)
  pure $
    testGroup
      "unknown request type"
      [ inputsAreConsumed inputs,
        testResponse
          ["Unknown Request type \"bla\"."]
          outputs,
        storeIsEmpty store
      ]

simulateConnection :: (Input WS -> Stream WS e (SubM e)) -> IO (Input WS, SimulationState e)
simulateConnection api = do
  input <- connect
  state <- simulate api input (SimulationState [apolloInit] [] empty)
  pure (input, state)

testConnectionInit ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testConnectionInit api = do
  (input, SimulationState {inputs, outputs, store}) <- simulateConnection api
  pure $
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

simulateStart :: (Input WS -> Stream WS e (SubM e)) -> IO (Input WS, SimulationState e)
simulateStart api = do
  (input, SimulationState _ o s) <- simulateConnection api
  state0 <- simulate api input (SimulationState [startSub "1", startSub "5"] o s)
  state <- simulate api input state0
  pure (input, state)

testSubscriptionStart ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testSubscriptionStart api = do
  (input, SimulationState {inputs, outputs, store}) <- simulateStart api
  pure $
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
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testSubscriptionStop api = do
  (input, SimulationState _ o s) <- simulateStart api
  SimulationState {inputs, outputs, store} <- simulate api input (SimulationState [apolloStop "1"] o s)
  pure $
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
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  IO TestTree
testApolloRequest api = do
  unknownType <- testUnknownType api
  connection_init <- testConnectionInit api
  start <- testSubscriptionStart api
  stop <- testSubscriptionStop api
  return $ testGroup "ApolloRequest" [unknownType, connection_init, start, stop]
