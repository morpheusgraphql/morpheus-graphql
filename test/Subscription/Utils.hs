{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.Utils
  ( TrailState (..),
    trail,
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
  )
where

import Control.Monad.State.Lazy
  ( StateT,
    runStateT,
    state,
  )
import Data.ByteString.Lazy.Char8
  ( ByteString,
  )
import Data.List
  ( sort,
  )
import Data.Maybe
  ( isJust,
  )
import Data.Morpheus.Types
  ( Input,
    Stream,
  )
import Data.Morpheus.Types.Internal.AST
  ( Name,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( ClientConnectionStore,
    GQLChannel (..),
    Input (..),
    Scope (..),
    WS,
    connectionSessionIds,
    publish,
    runStreamWS,
    toList,
  )
import Data.Semigroup
  ( (<>),
  )
import Test.Tasty
  ( TestTree,
  )
import Test.Tasty.HUnit
  ( assertEqual,
    assertFailure,
    testCase,
  )

data TrailState e = TrailState
  { inputs :: [ByteString],
    outputs :: [ByteString],
    store :: ClientConnectionStore e (SubM e)
  }

type Store e = ClientConnectionStore e (SubM e)

type SubM e = StateT (TrailState e) IO

addOutput :: ByteString -> TrailState e -> ((), TrailState e)
addOutput x (TrailState i xs st) = ((), TrailState i (xs <> [x]) st)

updateStore :: (Store e -> Store e) -> TrailState e -> ((), TrailState e)
updateStore up (TrailState i o st) = ((), TrailState i o (up st))

readInput :: TrailState e -> (ByteString, TrailState e)
readInput (TrailState (i : ins) o s) = (i, TrailState ins o s)
readInput (TrailState [] o s) = ("<Error>", TrailState [] o s)

wsApp ::
  (Input WS -> Stream WS e (SubM e)) ->
  Input WS ->
  SubM e ()
wsApp api input =
  runStreamWS
    ScopeWS
      { update = state . updateStore,
        listener = state readInput,
        callback = state . addOutput
      }
    (api input)

simulatePublish ::
  (GQLChannel e, Eq (StreamChannel e)) =>
  e ->
  TrailState e ->
  IO (TrailState e)
simulatePublish event s = snd <$> runStateT (publish event (store s)) s

trail ::
  (Input WS -> Stream WS e (SubM e)) ->
  Input WS ->
  TrailState e ->
  IO (TrailState e)
trail api input = fmap snd . runStateT (wsApp api input)

expectedResponse :: [ByteString] -> [ByteString] -> IO ()
expectedResponse expected value
  | expected == value = return ()
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

storeIsEmpty :: Store e -> TestTree
storeIsEmpty cStore
  | null (toList cStore) =
    testCase "connectionStore: is empty" $ return ()
  | otherwise =
    testCase "connectionStore: is empty"
      $ assertFailure
      $ " must be empty but "
        <> show
          cStore

storedSingle :: Store e -> TestTree
storedSingle cStore
  | length (toList cStore) == 1 =
    testCase "stored single connection" $ return ()
  | otherwise =
    testCase "stored single connection"
      $ assertFailure
      $ "connectionStore must store single connection"
        <> show
          cStore

stored :: Input WS -> Store e -> TestTree
stored (Init uuid) cStore
  | isJust (lookup uuid (toList cStore)) =
    testCase "stored connection" $ return ()
  | otherwise =
    testCase "stored connection"
      $ assertFailure
      $ " must store connection \"" <> show uuid <> "\" but stored: "
        <> show
          cStore

storeSubscriptions ::
  Input WS ->
  [Name] ->
  Store e ->
  TestTree
storeSubscriptions
  (Init uuid)
  sids
  cStore =
    checkSession (lookup uuid (toList cStore))
    where
      checkSession (Just conn)
        | sort sids == sort (connectionSessionIds conn) =
          testCase "stored subscriptions" $ return ()
        | otherwise =
          testCase "stored subscriptions"
            $ assertFailure
            $ " must store subscriptions with id \"" <> show sids <> "\" but stored: "
              <> show
                (connectionSessionIds conn)
      checkSession _ =
        testCase "stored connection"
          $ assertFailure
          $ " must store connection \"" <> show uuid <> "\" but: "
            <> show
              cStore

apolloStart :: ByteString -> ByteString -> ByteString
apolloStart query sid = "{\"id\":\"" <> sid <> "\",\"type\":\"start\",\"payload\":{\"variables\":{},\"operationName\":\"MySubscription\",\"query\":\"" <> query <> "\"}}"

apolloStop :: ByteString -> ByteString
apolloStop x = "{\"id\":\"" <> x <> "\",\"type\":\"stop\"}"

apolloRes :: ByteString -> ByteString -> ByteString
apolloRes sid value = "{\"id\":\"" <> sid <> "\",\"type\":\"data\",\"payload\":{\"data\":" <> value <> "}}"

apolloInit :: ByteString
apolloInit = "{ \"type\":\"connection_init\" }"
