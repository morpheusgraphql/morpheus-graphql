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
    storeSubscription,
  )
where

import Control.Monad.State.Lazy
  ( StateT,
    runStateT,
    state,
  )
import Data.ByteString.Lazy.Char8 (ByteString)
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
    Input (..),
    Scope (..),
    WS,
    connectionSessionIds,
    runStreamWS,
    toList,
  )
import Data.Semigroup ((<>))
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
      $ " must store connection \"" <> show uuid <> "\" but: "
        <> show
          cStore

storeSubscription ::
  Input WS ->
  Name ->
  Store e ->
  TestTree
storeSubscription (Init uuid) sid cStore = checkSession (lookup uuid (toList cStore))
  where
    checkSession (Just conn)
      | sid `elem` connectionSessionIds conn =
        testCase "stored subscription" $ return ()
      | otherwise =
        testCase "stored subscription"
          $ assertFailure
          $ " must store subscription \"" <> show sid <> "\" but: "
            <> show
              conn
    checkSession _ =
      testCase "stored connection"
        $ assertFailure
        $ " must store connection \"" <> show uuid <> "\" but: "
          <> show
            cStore
