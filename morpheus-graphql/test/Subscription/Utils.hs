{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Subscription.Utils
  ( SimulationState (..),
    simulate,
    expectedResponse,
    SubM,
    testResponse,
    inputsAreConsumed,
    storeIsEmpty,
    storedSingle,
    stored,
    storeSubscriptions,
    simulatePublish,
    apolloStart,
    apolloStop,
    apolloRes,
    apolloInit,
    apolloConnectionAck,
    apolloConnectionErr,
    apolloPing,
    apolloPong,
    testSimulation,
  )
where

import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.List (lookup)
import Data.Morpheus
  ( App,
  )
import Data.Morpheus.Subscriptions
  ( Event,
  )
import Data.Morpheus.Subscriptions.Internal
  ( ApiContext (..),
    ClientConnectionStore,
    Input (..),
    SUB,
    connect,
    connectionSessionIds,
    empty,
    publish,
    runStreamWS,
    streamApp,
    toList,
  )
import Relude hiding
  ( ByteString,
    empty,
    toList,
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    assertFailure,
    testCase,
  )

data SimulationState e = SimulationState
  { inputs :: [ByteString],
    outputs :: [ByteString],
    store :: ClientConnectionStore e (SubM e)
  }

type Store e = ClientConnectionStore e (SubM e)

type SubM e = StateT (SimulationState e) IO

addOutput :: ByteString -> SimulationState e -> ((), SimulationState e)
addOutput x (SimulationState i xs st) = ((), SimulationState i (xs <> [x]) st)

mockUpdateStore :: (Store e -> Store e) -> SimulationState e -> ((), SimulationState e)
mockUpdateStore up (SimulationState i o st) = ((), SimulationState i o (up st))

readInput :: SimulationState e -> (ByteString, SimulationState e)
readInput (SimulationState (i : ins) o s) = (i, SimulationState ins o s)
readInput (SimulationState [] o s) = ("<Error>", SimulationState [] o s)

wsApp ::
  (Eq ch, Hashable ch) =>
  App (Event ch e) (SubM (Event ch e)) ->
  Input SUB ->
  SubM (Event ch e) ()
wsApp app =
  runStreamWS
    SubContext
      { updateStore = state . mockUpdateStore,
        listener = state readInput,
        callback = state . addOutput
      }
    . streamApp app

simulatePublish ::
  (Eq ch, Show ch, Hashable ch) =>
  Event ch con ->
  SimulationState (Event ch con) ->
  IO (SimulationState (Event ch con))
simulatePublish event s = snd <$> runStateT (publish event (store s)) s

simulate ::
  (Eq ch, Hashable ch) =>
  App (Event ch con) (SubM (Event ch con)) ->
  Input SUB ->
  SimulationState (Event ch con) ->
  IO (SimulationState (Event ch con))
simulate _ _ s@SimulationState {inputs = []} = pure s
simulate api input s = runStateT (wsApp api input) s >>= simulate api input . snd

testSimulation ::
  (Eq ch, Hashable ch) =>
  (Input SUB -> SimulationState (Event ch con) -> TestTree) ->
  [ByteString] ->
  App (Event ch con) (SubM (Event ch con)) ->
  IO TestTree
testSimulation test simInputs api = do
  input <- connect
  s <- simulate api input (SimulationState simInputs [] empty)
  pure $ test input s

expectedResponse :: [ByteString] -> [ByteString] -> IO ()
expectedResponse expected value
  | expected == value = pure ()
  | otherwise =
      assertFailure $ "expected: \n " <> show expected <> " \n but got: \n " <> show value

testResponse :: [ByteString] -> [ByteString] -> TestTree
testResponse expected =
  testCase
    "expected response"
    . expectedResponse
      expected

inputsAreConsumed :: [ByteString] -> TestTree
inputsAreConsumed =
  testCase "inputs are consumed"
    . assertEqual
      "input stream should be consumed"
      []

storeIsEmpty :: (Show ch) => Store (Event ch con) -> TestTree
storeIsEmpty cStore
  | null (toList cStore) =
      testCase "connectionStore: is empty" $ pure ()
  | otherwise =
      testCase "connectionStore: is empty"
        $ assertFailure
        $ " must be empty but "
        <> show
          cStore

storedSingle :: (Show ch) => Store (Event ch con) -> TestTree
storedSingle cStore
  | length (toList cStore) == 1 =
      testCase "stored single connection" $ pure ()
  | otherwise =
      testCase "stored single connection"
        $ assertFailure
        $ "connectionStore must store single connection, but stored: "
        <> show
          cStore

stored :: (Show ch) => Input SUB -> Store (Event ch con) -> TestTree
stored (InitConnection uuid) cStore
  | isJust (lookup uuid (toList cStore)) =
      testCase "stored connection" $ pure ()
  | otherwise =
      testCase "stored connection"
        $ assertFailure
        $ " must store connection \""
        <> show uuid
        <> "\" but stored: "
        <> show
          cStore

storeSubscriptions ::
  (Show ch) =>
  Input SUB ->
  [Text] ->
  Store (Event ch con) ->
  TestTree
storeSubscriptions
  (InitConnection uuid)
  sids
  cStore =
    checkSession (lookup uuid (toList cStore))
    where
      checkSession (Just conn)
        | sort sids == sort (connectionSessionIds conn) =
            testCase "stored subscriptions" $ pure ()
        | otherwise =
            testCase "stored subscriptions"
              $ assertFailure
              $ " must store subscriptions with id \""
              <> show sids
              <> "\" but stored: "
              <> show
                (connectionSessionIds conn)
      checkSession _ =
        testCase "stored connection"
          $ assertFailure
          $ " must store connection \""
          <> show uuid
          <> "\" but: "
          <> show
            cStore

apolloStart :: ByteString -> ByteString -> ByteString
apolloStart query sid =
  "{\"id\":\"" <> sid <> "\",\"type\":\"subscribe\",\"payload\":{\"variables\":{},\"operationName\":\"MySubscription\",\"query\":\"" <> query <> "\"}}"

apolloStop :: ByteString -> ByteString
apolloStop x = "{\"id\":\"" <> x <> "\",\"type\":\"complete\"}"

apolloRes :: ByteString -> ByteString -> ByteString
apolloRes sid value = "{\"id\":\"" <> sid <> "\",\"type\":\"next\",\"payload\":{\"data\":" <> value <> "}}"

apolloInit :: ByteString
apolloInit = "{\"type\":\"connection_init\"}"

apolloConnectionAck :: ByteString
apolloConnectionAck = "{\"type\":\"connection_ack\"}"

apolloConnectionErr :: ByteString
apolloConnectionErr = "{\"type\":\"connection_error\"}"

apolloPing :: ByteString
apolloPing = "{\"type\":\"ping\"}"

apolloPong :: ByteString
apolloPong = "{\"type\":\"pong\"}"
